{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.EGV where

import Control.Applicative (Alternative(empty))
import Control.Enumerable (Sized(aconcat), Enumerable(enumerate), share, global)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Typeable (Typeable)
import Test.Feat (Enumerate, c0, c1, c2, deriveEnumerable)


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


instance Enumerable Type where
  enumerate = share $ aconcat [ c0 Unit , c2 (:->) , c2 (:+) , c2 (:*) , c1 Emb ]

instance Enumerable Sess where
  enumerate = share $ aconcat [ c2 (:!) , c2 (:?) , c0 End ]


-- * Variables

data Z
  deriving (Typeable, Eq, Show)

data S n
  = FZ
  | FS n
  deriving (Typeable, Eq, Show)


instance Enumerable Z where
  enumerate = share $ empty

instance Enumerable n => Enumerable (S n) where
  enumerate = share $ aconcat [ c0 FZ , c1 FS ]



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
  | Case (Term n) (Term (S n)) (Term (S n)) Type Type
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

-- check if n has been used.
notIn :: Int -> Env -> Bool
n `notIn` env = IM.lookup n env == Nothing

-- extend the environment with a new type
extendWith :: Env -> Type -> Env
extendWith env a =  IM.insert 0 a (IM.mapKeysMonotonic succ env)

-- shrink the environment by dropping the first n types
shrinkBy :: Env -> Int -> Env
shrinkBy env n = IM.mapKeysMonotonic (subtract n) env

check :: Fin n => Env -> Type -> Term n -> Bool
check = ((snd .) .) . go
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
        env4        = env3 `shrinkBy` 2

    go env (a :+ _) (Inl x) = go env a x
    go env (_ :+ b) (Inr x) = go env b x

    go env c (Case x y z a b) = (env4, ih1 && ih2a && ih2b && env3Same)
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

    go env _ Raise = (env, IM.null env)

    go env _ _ = (env, False)

checkClosed :: Type -> Term Z -> Bool
checkClosed = check IM.empty


-- * Configurations

data Flag
  = Main
  | Child

data Conf n
  = New (Conf (S n))
  | Par (Conf n) (Conf n)
  | Thd Flag (Term n)
  | Halt
  | Zap n
  | Buf n [Term n] n [Term n]


-- * Reduction

ren :: Fin n => (n -> m) -> (Term n -> Term m)
ren r (Var n)             = Var (r n)
ren r (Lam x)             = Lam (ren (ext r) x)
ren r (App x y a)         = App (ren r x) (ren r y) a
ren _ TT                  = TT
ren r (LetTT x y)         = LetTT (ren r x) (ren r y)
ren r (Pair x y)          = Pair (ren r x) (ren r y)
ren r (LetPair x y a b)   = LetPair (ren r x) (ren (ext (ext r)) y) a b
ren r (Inl x)             = Inl (ren r x)
ren r (Inr x)             = Inr (ren r x)
ren r (Case x y z a b)    = Case (ren r x) (ren (ext r) y) (ren (ext r) z) a b
ren r (Fork x)            = Fork (ren r x)
ren r (Send x y a)        = Send (ren r x) (ren r y) a
ren r (Recv x)            = Recv (ren r x)
ren r (Close x)           = Close (ren r x)
ren r (Cancel x s)        = Cancel (ren r x) s
ren r (Try x y z a)       = Try (ren r x) (ren (ext r) y) (ren r z) a
ren _ Raise               = Raise


sub :: (Fin n, Fin m) => (n -> Term m) -> (Term n -> Term m)
sub r (Var n)             = r n
sub r (Lam x)             = Lam (sub (exts r) x)
sub r (App x y a)         = App (sub r x) (sub r y) a
sub _ TT                  = TT
sub r (LetTT x y)         = LetTT (sub r x) (sub r y)
sub r (Pair x y)          = Pair (sub r x) (sub r y)
sub r (LetPair x y a b)   = LetPair (sub r x) (sub (exts (exts r)) y) a b
sub r (Inl x)             = Inl (sub r x)
sub r (Inr x)             = Inr (sub r x)
sub r (Case x y z a b)    = Case (sub r x) (sub (exts r) y) (sub (exts r) z) a b
sub r (Fork x)            = Fork (sub r x)
sub r (Send x y a)        = Send (sub r x) (sub r y) a
sub r (Recv x)            = Recv (sub r x)
sub r (Close x)           = Close (sub r x)
sub r (Cancel x s)        = Cancel (sub r x) s
sub r (Try x y z a)       = Try (sub r x) (sub (exts r) y) (sub r z) a
sub _ Raise               = Raise


subst :: Fin n => Term (S n) -> Term n -> Term n
subst x y = sub r x
  where
    r FZ     = y
    r (FS z) = Var z


isValue :: Term n -> Bool
isValue (Var _)    = True
isValue (Lam x)    = isValue x
isValue TT         = True
isValue (Pair x y) = isValue x && isValue y
isValue (Inl x)    = isValue x
isValue (Inr x)    = isValue x
isValue _          = False


stepTerm :: Fin n => Term n -> Maybe (Term n)
stepTerm (App (Lam x) y _)
  | isValue y = Just $ subst x y
stepTerm (LetTT TT x) = Just $ x
stepTerm (LetPair (Pair x y) z _ _)
  | isValue x && isValue y = Just $ subst (subst z (ren FS x)) y
stepTerm (Case (Inl x) y _ _ _)
  | isValue x = Just $ subst y x
stepTerm (Case (Inr x) _ z _ _)
  | isValue x = Just $ subst z x
stepTerm (Try x y z _)
  | isValue x = Just $ subst y z
stepTerm (App x y a)
  | isValue x = App <$> stepTerm x <*> pure y <*> pure a
  | otherwise = App <$> pure x <*> stepTerm y <*> pure a
stepTerm (LetTT x y) = LetTT <$> stepTerm x <*> pure y
stepTerm (LetPair x y a b) = LetPair <$> stepTerm x <*> pure y <*> pure a <*> pure b
stepTerm (Pair x y)
  | isValue y = Pair <$> stepTerm x <*> pure y
  | isValue x = Pair <$> pure x <*> stepTerm y
stepTerm (Inl x) = Inl <$> stepTerm x
stepTerm (Inr x) = Inr <$> stepTerm x
stepTerm (Case x y z a b) =
  Case <$> stepTerm x <*> pure y <*> pure z <*> pure a <*> pure b
stepTerm (Fork x) = Fork <$> stepTerm x
stepTerm (Send x y a)
  | isValue x = Send <$> pure x <*> stepTerm y <*> pure a
  | otherwise = Send <$> stepTerm x <*> pure y <*> pure a
stepTerm (Recv x) = Recv <$> stepTerm x
stepTerm (Close x) = Close <$> stepTerm x
stepTerm (Cancel x s) = Cancel <$> stepTerm x <*> pure s
stepTerm (Try x y z a) = Try <$> stepTerm x <*> pure y <*> pure z <*> pure a
stepTerm _ = Nothing


-- * Finite types

class Eq n => Fin n where
  toInt :: n -> Int
  ext   :: (n -> m) -> (S n -> S m)
  exts  :: Fin m => (n -> Term m) -> (S n -> Term (S m))

instance Fin Z where
  toInt  x = x `seq` undefined
  ext  _ x = x `seq` undefined
  exts _ x = x `seq` undefined

instance Fin n => Fin (S n) where
  toInt  FZ     = 0
  toInt  (FS x) = succ (toInt x)
  ext  _ FZ     = FZ
  ext  r (FS x) = FS (r x)
  exts _ FZ     = Var FZ
  exts r (FS x) = ren FS (r x)
