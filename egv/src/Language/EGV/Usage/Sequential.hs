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
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.EGV.Usage.Sequential where



import           Control.Monad.Indexed (IxPointed(..), IxMonad(..), (>>>=))
import           Control.Monad.Indexed.State (IxMonadState(..), IxState(..))
import           Data.Bool (bool)
import           Data.Set (Set)
import qualified Data.Set as S
import           Language.EGV.Base
import           Prelude hiding (Monad(..))


data UCS c n = (Ord c, Ord n) => UCS
  { termsUnused :: Set n
  , chansUnused :: Set c
  }


instance Eq (UCS c n) where
  UCS termsUsed1 chansUsed1 == UCS termsUsed2 chansUsed2 =
    termsUsed1 == termsUsed2 && chansUsed1 == chansUsed2


emptyUCS :: UCS Z Z
emptyUCS = UCS S.empty S.empty


checkUsage :: Term Z Z -> Bool
checkUsage tm = fst (runIxState (checkUsage' tm) emptyUCS)


checkUsage' :: (Ord c, Ord n, IxMonadState m) => Term c n -> m (UCS c n) (UCS c n) Bool
checkUsage' (Var n) = useVar (Right n)
checkUsage' (Chan c) = useVar (Left c)
checkUsage' (Lam body) = do
  newVar
  bodyOk <- checkUsage' body
  varUsed <- checkVarUsed
  return (bodyOk && varUsed)
checkUsage' (App fun arg _argTy) = checkUsage' fun <&&> checkUsage' arg
checkUsage' TT = return True
checkUsage' (LetTT tt body) = checkUsage' tt <&&> checkUsage' body
checkUsage' (Pair fst snd) = checkUsage' fst <&&> checkUsage' snd
checkUsage' (LetPair pair body _fstTy _sndTy) =
  checkUsage' pair <&&> do
    newVar
    newVar
    bodyOk <- checkUsage' body
    fstUsed <- checkVarUsed
    sndUsed <- checkVarUsed
    return (bodyOk && fstUsed && sndUsed)
checkUsage' (Inl tm) = checkUsage' tm
checkUsage' (Inr tm) = checkUsage' tm
checkUsage' (Case sum inlBody inrBody _inlTy _inrTy) =
  checkUsage' sum <&&> do
    newVar
    bodiesOk <- checkSum (checkUsage' inlBody) (checkUsage' inrBody)
    varsUsed <- checkVarUsed
    return (bodiesOk && varsUsed)
checkUsage' (Fork tm) = checkUsage' tm
checkUsage' (Send msg chan _msgTy) = checkUsage' msg <&&> checkUsage' chan
checkUsage' (Recv chan) = checkUsage' chan
checkUsage' (Close chan) = checkUsage' chan
checkUsage' (Cancel chan _chanTy) = checkUsage' chan
checkUsage' (Try tm body exn _tmTy) =
  checkUsage' tm <&&> do
    checkSum (do newVar
                 bodyOk <- checkUsage' body
                 varUsed <- checkVarUsed
                 return (bodyOk && varUsed))
             (do checkUsage' exn)
checkUsage' Raise = return True



-- * Helper functions

newVar :: (IxMonadState m) => m (UCS c n) (UCS c (S n)) ()
newVar = do
  UCS{..} <- iget
  iput UCS{ termsUnused = S.insert FZ (S.mapMonotonic FS termsUnused), .. }
  return ()


useVar :: (IxMonadState m) => Either c n -> m (UCS c n) (UCS c n) Bool
useVar (Left c) = do
  UCS{..} <- iget
  case S.lookupIndex c chansUnused of
    Just ix -> do iput UCS{ chansUnused = S.deleteAt ix chansUnused, .. }; return True
    Nothing -> return False
useVar (Right n) = do
  UCS{..} <- iget
  case S.lookupIndex n termsUnused of
    Just ix -> do iput UCS{ termsUnused = S.deleteAt ix termsUnused, .. }; return True
    Nothing -> return False

checkVarUsed :: (Ord n, IxMonadState m) => m (UCS c (S n)) (UCS c n) Bool
checkVarUsed = do
  UCS{..} <- iget
  iput UCS{ termsUnused = S.mapMonotonic fpred (S.delete FZ termsUnused), .. }
  return (S.notMember FZ termsUnused)

checkSum :: (Eq c, Eq n, IxMonadState m)
         => m (UCS c n) (UCS c n) Bool
         -> m (UCS c n) (UCS c n) Bool
         -> m (UCS c n) (UCS c n) Bool
checkSum check1 check2 = do
  ucs  <- iget   -- Backup the starting state.
  ok1  <- check1 -- Run the first check.
  ucs1 <- iget   -- Log the state after the first check.
  iput ucs       -- Restore the starting state.
  ok2  <- check2 -- Run the second check.
  ucs2 <- iget   -- Log the state after the second check.
  return (ok1 && ok2 && ucs1 == ucs2)

(<&&>) :: (IxMonad m) => m i i Bool -> m i i Bool -> m i i Bool
b1 <&&> b2 = b1 >>>= bool (return False) b2


-- * Rebindable syntax

(>>=)  = (>>>=)
(>>)   = (. const) . (>>>=)
return = ireturn
fail   = error
