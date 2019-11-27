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



import           Data.Coolean
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Language.EGV.Base
import           Language.EGV.Typing



-- * Parallel-but-strict usage checking

-- |Contains sets of used term and channel names.
data Usage c n = (Ord c, Ord n) => Usage
  { termsUsed :: Set n
  , chansUsed :: Set c
  }


instance Eq (Usage c n) where
  Usage termsUsed1 chansUsed1 == Usage termsUsed2 chansUsed2 =
    termsUsed1 == termsUsed2 && chansUsed1 == chansUsed2


-- |The empty usage.
emptyUsage :: (Ord c, Ord n) => Usage c n
emptyUsage = Usage S.empty S.empty


-- |Shrink a usage set by subtracting one from each DeBruijn index.
-- Removes `FZ`, and fails if `FZ` isn't a member of the input set.
shrinkUsage :: Set (S n) -> Maybe (Set n)
shrinkUsage termsUsed = do
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
unionUsage :: Usage c n -> Usage c n -> Maybe (Usage c n)
unionUsage (Usage termsUsed1 chansUsed1) (Usage termsUsed2 chansUsed2) =
  Usage <$> unionIfDisjoint termsUsed1 termsUsed2 <*> unionIfDisjoint chansUsed1 chansUsed2


-- |Check the usages of two terms, and return their union if they are disjoint.
checkAndUnionUsage :: (Ord c, Ord n) => Term c n -> Term c n -> Maybe (Usage c n)
checkAndUnionUsage tm1 tm2 = do
  usage1 <- checkBoundUsage tm1
  usage2 <- checkBoundUsage tm2
  unionUsage usage1 usage2


-- |Check if the usage of bound variables is linear. Returns the usage for any free names.
checkBoundUsage :: (Ord c, Ord n) => Term c n -> Maybe (Usage c n)
checkBoundUsage (Var n) =
  return $ Usage (S.singleton n) S.empty

checkBoundUsage (Chan c) =
  return $ Usage S.empty (S.singleton c)

checkBoundUsage (Lam body) = do
  Usage{..} <- checkBoundUsage body
  termsUsed' <- shrinkUsage termsUsed
  return $ Usage termsUsed' chansUsed

checkBoundUsage (App fun arg _argTy) =
  checkAndUnionUsage fun arg

checkBoundUsage TT =
  return emptyUsage

checkBoundUsage (LetTT tt body) =
  checkAndUnionUsage tt body

checkBoundUsage (Pair fst snd) =
  checkAndUnionUsage fst snd

checkBoundUsage (LetPair pair body _fstTy _sndTy) = do
  usagePair <- checkBoundUsage pair
  Usage termsUsedBody chansUsedBody <- checkBoundUsage body
  termsUsedBody' <- shrinkUsage =<< shrinkUsage termsUsedBody
  let usageBody = Usage termsUsedBody' chansUsedBody
  unionUsage usagePair usageBody

checkBoundUsage (Inr tm) =
  checkBoundUsage tm

checkBoundUsage (Inl tm) =
  checkBoundUsage tm

checkBoundUsage (Case sum inlBody inrBody _inlTy _inrTy) = do
  usageSum <- checkBoundUsage sum
  Usage termsUsedInlBody chansUsedInlBody <- checkBoundUsage inlBody
  termsUsedInlBody' <- shrinkUsage termsUsedInlBody
  let usageInlBody = Usage termsUsedInlBody' chansUsedInlBody
  Usage termsUsedInrBody chansUsedInrBody <- checkBoundUsage inrBody
  termsUsedInrBody' <- shrinkUsage termsUsedInrBody
  let usageInrBody = Usage termsUsedInrBody' chansUsedInrBody
  if usageInlBody == usageInrBody
    then unionUsage usageSum usageInlBody -- or usageInrBody
    else Nothing

checkBoundUsage (Fork tm) =
  checkBoundUsage tm

checkBoundUsage (Send msg chan _msgTy) =
  checkAndUnionUsage msg chan

checkBoundUsage (Recv chan) =
  checkBoundUsage chan

checkBoundUsage (Close chan) =
  checkBoundUsage chan

checkBoundUsage (Cancel chan _chanTy) =
  checkBoundUsage chan

checkBoundUsage (Try tm body exn _tmTy) = do
  usageTm <- checkBoundUsage tm
  Usage termsUsedBody chansUsedBody <- checkBoundUsage body
  termsUsedBody' <- shrinkUsage termsUsedBody
  let usageBody = Usage termsUsedBody' chansUsedBody
  usageExn <- checkBoundUsage exn
  if usageBody == usageExn
    then unionUsage usageTm usageBody -- or usageExn
    else Nothing

checkBoundUsage Raise =
  return emptyUsage


-- |Check if the usage in a term is linear. Requires that free names are used linearly.
checkUsage :: forall c n. (Fin c, Fin n) => Term c n -> Bool
checkUsage tm = maybe False usagePred (checkBoundUsage tm)
  where
    usagePred :: Usage c n -> Bool
    usagePred Usage{..} =
      termsUsed == S.fromAscList allFin && chansUsed == S.fromAscList allFin


-- * Check linear types

-- |Check if the term is well-typed and linear.
checkTypeAndUsage :: Type -> Term Z Z -> Cool
checkTypeAndUsage ty tm = checkType emptyTCS ty tm !&& checkUsage tm
