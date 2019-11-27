{-# OPTIONS -fno-warn-name-shadowing   #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Language.EGV.Base where



import Control.Enumerable
import Data.Typeable      (Typeable)



-- * Finite types

data Z
  deriving (Typeable, Eq, Ord, Show)

absurd :: Z -> a
absurd i = i `seq` error "instance of empty type Z"


data S n
  = FZ
  | FS n
  deriving (Functor, Typeable, Eq, Ord, Show)

fpred :: S n -> n
fpred FZ     = error "predecessor of finite type zero"
fpred (FS i) = i


instance Enumerable Z where
  enumerate = share $ aconcat []

instance Enumerable n => Enumerable (S n) where
  enumerate = share $ aconcat [ c0 FZ , c1 FS ]


class Ord n => Fin n where
  toInt  :: n -> Int
  allFin :: [n]

instance Fin Z where
  toInt :: Z -> Int
  toInt = absurd

  allFin :: [Z]
  allFin = []

instance (Fin n) => Fin (S n) where
  toInt :: S n -> Int
  toInt FZ = 0
  toInt (FS i) = 1 + toInt i

  allFin :: [S n]
  allFin = FZ : map FS allFin



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


-- |Returns the dual session type.
dual :: Sess -> Sess
dual (a :! s) = a :? dual s
dual (a :? s) = a :! dual s
dual End = End


instance Enumerable Type where
  enumerate = share $ aconcat
    [ pay . c0 $ Unit
    , pay . c2 $ (:->)
    , pay . c2 $ (:+)
    , pay . c2 $ (:*)
    , pay . c1 $ Emb
    ]

instance Enumerable Sess where
  enumerate = share $ aconcat
    [ pay . c2 $ (:!)
    , pay . c2 $ (:?)
    , pay . c0 $ End
    ]



-- * Terms

data Term c n
  = Var n
  | Chan c
  | Lam (Term c (S n))
  | App (Term c n) (Term c n) Type
  | TT
  | LetTT (Term c n) (Term c n)
  | Pair (Term c n) (Term c n)
  | LetPair (Term c n) (Term c (S (S n))) Type Type
  | Inl (Term c n)
  | Inr (Term c n)
  | Case (Term c n) (Term c (S n)) (Term c (S n)) Type Type
  | Fork (Term c n)
  | Send (Term c n) (Term c n) Type
  | Recv (Term c n)
  | Close (Term c n)
  | Cancel (Term c n) Sess
  | Try (Term c n) (Term c (S n)) (Term c n) Type
  | Raise
  deriving (Functor, Typeable, Eq, Show)


instance Enumerable n => Enumerable (Term Z n) where
  enumerate = share $ aconcat
    [ pay . c1 $ Var
    , pay . c1 $ Lam
    , pay . c3 $ App
    , pay . c0 $ TT
    , pay . c2 $ LetTT
    , pay . c2 $ Pair
    , pay . c4 $ LetPair
    , pay . c1 $ Inl
    , pay . c1 $ Inr
    , pay . c5 $ Case
    , pay . c1 $ Fork
    , pay . c3 $ Send
    , pay . c1 $ Recv
    , pay . c1 $ Close
    , pay . c2 $ Cancel
    , pay . c4 $ Try
    , pay . c0 $ Raise ]


-- * Configurations

data Conf c
  = New (Conf (S c))
  | Conf (Threads c)

data Threads c = Threads
  { busy    :: [Thread c]       -- ^ Thread threads
  , forking :: [Thread c]       -- ^ Forking threads
  , blocked :: [(c, Blocked c)] -- ^ Blocked threads
  , values  :: [Thread c]       -- ^ Values
  }

data Flag
  = Main
  | Child

data Thread c
  = Thread Flag (Term c Z)

data Blocked c
  = Blocked (Thread c)
  | Ready (Thread c) (Thread c)
