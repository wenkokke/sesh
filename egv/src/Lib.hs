{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lib where

import Control.Enumerable
import Control.Search
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Test.Feat
import Test.Feat.Access (valuesWith)


-- * Types

data Type
  = Unit
  | Type :-> Type
  | Type :+  Type
  | Type :*  Type
  | Emb Sess
  deriving (Typeable, Eq, Show)

data Sess
  = Type :! Sess
  | Type :? Sess
  | End
  deriving (Typeable, Eq, Show)

deriveEnumerable ''Type
deriveEnumerable ''Sess


-- * Variables

data Z
  deriving (Typeable, Eq, Show)

data S n
  = FZ
  | FS n
  deriving (Typeable, Eq, Show)

deriveEnumerable ''Z

instance Enumerable n => Enumerable (S n) where
  enumerate = share $ aconcat [ c0 FZ , c1 FS ]

class Fin n where
  toInt :: n -> Int

instance Fin Z where
  toInt x = x `seq` undefined

instance Fin n => Fin (S n) where
  toInt FZ = 0
  toInt (FS x) = succ (toInt x)


-- * Terms

data Term n
  = Var n
  | Lam (Term (S n))
  | App (Term n) (Term n) Type
  | TT
  | LetTT (Term n) (Term n)
  | Pair (Term n) (Term n)
  | LetPair (Term n) (Term (S (S n))) Type Type
  | Inl (Term n)
  | Inr (Term n)
  | CaseSum (Term n) (Term (S n)) (Term (S n)) Type Type
  | Fork (Term n)
  | Send (Term n) (Term n) Type
  | Recv (Term n)
  | Close (Term n)
  | Cancel (Term n) Sess
  | Try (Term n) (Term (S n)) (Term n) Type
  | Raise
  deriving (Typeable, Eq, Show)

deriveEnumerable ''Term

closed :: Enumerate (Term Z)
closed = global


-- * Type checking

dual :: Sess -> Sess
dual (a :! s) = a :? dual s
dual (a :? s) = a :! dual s
dual End = End


type Env = IntMap Type

-- |Check if the variable |n| has been used.
notIn :: Int -> Env -> Bool
n `notIn` env = IM.lookup n env == Nothing

-- |Extend the environment with a new type.
extendWith :: Env -> Type -> Env
extendWith env a =  IM.insert 0 a (IM.mapKeysMonotonic succ env)

-- |Shrink the environment by dropping the first |n| variables.
shrinkBy :: Env -> Int -> Env
shrinkBy env n = IM.mapKeysMonotonic (subtract n) env

check :: Fin n => Env -> Type -> Term n -> Bool
check env a x = snd (go env a x)
  where
    go :: Fin n => Env -> Type -> Term n -> (Env, Bool)
    go env a (Var n) = (env1, hasType)
      where
        indx    = toInt n
        env1    = IM.delete indx env
        hasType = IM.lookup indx env == Just a

    go env (a :-> b) (Lam x) = (env3, ih && varUsed)
      where
        env1       = env `extendWith` a
        (env2, ih) = go env1 b x
        varUsed    = 0 `notIn` env2
        env3       = env2 `shrinkBy` 1

    go env b (App x y a) = (env2, ih1 && ih2)
      where
        (env1, ih1) = go env (a :-> b) x
        (env2, ih2) = go env1 a y

    go env Unit TT = (env, IM.null env)

    go env a (LetTT x y) = (env2, ih1 && ih2)
      where
        (env1, ih1) = go env Unit x
        (env2, ih2) = go env1 a y

    go env (a :* b) (Pair x y) = (env2, ih1 && ih2)
      where
        (env1, ih1) = go env a x
        (env2, ih2) = go env1 b y

    go env c (LetPair x y a b) = (env4, ih1 && ih2)
      where
        (env1, ih1) = go env (a :* b) x
        env2        = env1 `extendWith` b `extendWith` a
        (env3, ih2) = go env2 c y
        varUsed1    = 0 `notIn` env3
        varUsed2    = 1 `notIn` env3
        env4        = env3 `shrinkBy` 2

    go env (a :+ b) (Inl x) = go env a x
    go env (a :+ b) (Inr x) = go env b x

    go env c (CaseSum x y z a b) = (env4, ih1 && ih2a && ih2b && env3Same)
      where
        (env1, ih1)   = go env (a :+ b) x
        env2a         = env1 `extendWith` a
        (env3a, ih2a) = go env2a c y
        env2b         = env1 `extendWith` b
        (env3b, ih2b) = go env2b c z
        env3Same      = env3a == env3b -- implies a and b were used
        env4          = env3a `shrinkBy` 1

    go env (Emb s) (Fork x) = go env (Emb (dual s) :-> Unit) x

    go env (Emb s) (Send x y a) = (env2, ih1 && ih2)
      where
        (env1, ih1) = go env a x
        (env2, ih2) = go env1 (Emb (a :! s)) y

    go env (a :* Emb s) (Recv x) = go env (Emb (a :? s)) x

    go env Unit (Close x) = go env (Emb End) x

    go env Unit (Cancel x s) = go env (Emb s) x

    go env b (Try x y z a) = (env4, ih1 && ih2a && ih2b && env4Same)
      where
        (env1, ih1)   = go env a x
        env2a         = env1 `extendWith` a
        (env3a, ih2a) = go env2a b y
        env2b         = env1
        (env3b, ih2b) = go env2b b z
        env4a         = env3a `shrinkBy` 1
        env4b         = env3b
        env4Same      = env4a == env4b -- implies a was used
        env4          = env4a

    go env a Raise = (env, IM.null env)

    go env _ _ = (env, False)

checkClosed :: Type -> Term Z -> Bool
checkClosed = check IM.empty


test s = search s (checkClosed Unit)
