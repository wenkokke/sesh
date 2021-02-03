{-# OPTIONS -fno-warn-name-shadowing   #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Language.EGV.Usage.Parallel where



import           Data.Set           (Set)
import qualified Data.Set           as S
import           Language.EGV.Base



-- * Parallel-but-strict usage checking

-- |Contains sets of used term and channel names.
data UCS c n = (Ord c, Ord n) => UCS
  { termsUsed :: Set n
  , chansUsed :: Set c
  }


instance Eq (UCS c n) where
  UCS termsUsed1 chansUsed1 == UCS termsUsed2 chansUsed2 =
    termsUsed1 == termsUsed2 && chansUsed1 == chansUsed2


-- |The empty usage.
emptyUCS :: (Ord c, Ord n) => UCS c n
emptyUCS = UCS S.empty S.empty


-- |Shrink a usage set by subtracting one from each DeBruijn index.
-- Removes `FZ`, and fails if `FZ` isn't a member of the input set.
shrinkUCS :: Set (S n) -> Maybe (Set n)
shrinkUCS termsUsed = do
  -- Remove the smallest element from termsUsed.
  -- NOTE: `i == FZ` iff `S.member FZ termsUsed`
  (i, termsUsed') <- S.minView termsUsed

  -- Update the remainder by stripping one FZ constructor.
  -- NOTE: this is only safe because we removed FZ
  let termsUsed'' = S.mapMonotonic fpred termsUsed'

  -- Check if `i == FZ`:
  --   Y: Return the updated usage set.
  --   N: Fail and return Nothing.
  case i of
    FZ   -> return termsUsed''
    FS _ -> Nothing


-- |Returns `Just` the union of two sets if they are disjoint, `Nothing` otherwise.
unionIfDisjoint :: (Ord n) => Set n -> Set n -> Maybe (Set n)
unionIfDisjoint xs ys
  | xs `S.disjoint` ys = Just (xs `S.union` ys)
  | otherwise          = Nothing


-- |Returns `Just` the union of two usages if they are disjoint, `Nothing` otherwise.
unionUCS :: UCS c n -> UCS c n -> Maybe (UCS c n)
unionUCS (UCS termsUsed1 chansUsed1) (UCS termsUsed2 chansUsed2) =
  UCS <$> unionIfDisjoint termsUsed1 termsUsed2 <*> unionIfDisjoint chansUsed1 chansUsed2


-- |Check the usages of two terms, and return their union if they are disjoint.
checkAndUnionUCS :: (Ord c, Ord n) => Term c n -> Term c n -> Maybe (UCS c n)
checkAndUnionUCS tm1 tm2 = do
  usage1 <- checkBoundUsage tm1
  usage2 <- checkBoundUsage tm2
  unionUCS usage1 usage2


-- |Check if the usage of bound variables is linear. Returns the usage for any free names.
checkBoundUsage :: (Ord c, Ord n) => Term c n -> Maybe (UCS c n)
checkBoundUsage (Var n) =
  return $ UCS (S.singleton n) S.empty

checkBoundUsage (Chan c) =
  return $ UCS S.empty (S.singleton c)

checkBoundUsage (Lam body) = do
  UCS{..} <- checkBoundUsage body
  termsUsed' <- shrinkUCS termsUsed
  return $ UCS termsUsed' chansUsed

checkBoundUsage (App fun arg _argTy) =
  checkAndUnionUCS fun arg

checkBoundUsage TT =
  return emptyUCS

checkBoundUsage (LetTT tt body) =
  checkAndUnionUCS tt body

checkBoundUsage (Pair fst snd) =
  checkAndUnionUCS fst snd

checkBoundUsage (LetPair pair body _fstTy _sndTy) = do
  usagePair <- checkBoundUsage pair
  UCS termsUsedBody chansUsedBody <- checkBoundUsage body
  termsUsedBody' <- shrinkUCS =<< shrinkUCS termsUsedBody
  let usageBody = UCS termsUsedBody' chansUsedBody
  unionUCS usagePair usageBody

checkBoundUsage (Inr tm) =
  checkBoundUsage tm

checkBoundUsage (Inl tm) =
  checkBoundUsage tm

checkBoundUsage (Case sum inlBody inrBody _inlTy _inrTy) = do
  usageSum <- checkBoundUsage sum
  UCS termsUsedInlBody chansUsedInlBody <- checkBoundUsage inlBody
  termsUsedInlBody' <- shrinkUCS termsUsedInlBody
  let usageInlBody = UCS termsUsedInlBody' chansUsedInlBody
  UCS termsUsedInrBody chansUsedInrBody <- checkBoundUsage inrBody
  termsUsedInrBody' <- shrinkUCS termsUsedInrBody
  let usageInrBody = UCS termsUsedInrBody' chansUsedInrBody
  if usageInlBody == usageInrBody
    then unionUCS usageSum usageInlBody -- or usageInrBody
    else Nothing

checkBoundUsage (Fork tm) =
  checkBoundUsage tm

checkBoundUsage (Send msg chan _msgTy) =
  checkAndUnionUCS msg chan

checkBoundUsage (Recv chan) =
  checkBoundUsage chan

checkBoundUsage (Close chan) =
  checkBoundUsage chan

checkBoundUsage (Cancel chan _chanTy) =
  checkBoundUsage chan

checkBoundUsage (Try tm body exn _tmTy) = do
  usageTm <- checkBoundUsage tm
  UCS termsUsedBody chansUsedBody <- checkBoundUsage body
  termsUsedBody' <- shrinkUCS termsUsedBody
  let usageBody = UCS termsUsedBody' chansUsedBody
  usageExn <- checkBoundUsage exn
  if usageBody == usageExn
    then unionUCS usageTm usageBody -- or usageExn
    else Nothing

checkBoundUsage Raise =
  return emptyUCS


-- |Check if the usage in a term is linear. Requires that free names are used linearly.
checkUsage :: forall c n. (Fin c, Fin n) => Term c n -> Bool
checkUsage tm = maybe False usagePred (checkBoundUsage tm)
  where
    usagePred :: UCS c n -> Bool
    usagePred UCS{..} =
      termsUsed == S.fromAscList allFin && chansUsed == S.fromAscList allFin
