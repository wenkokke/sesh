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
module Language.EGV.Eval where



import           Control.Enumerable
import           Data.Bifunctor
import           Data.Coolean
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Typeable      (Typeable)
import           Language.EGV.Base



-- * Term reduction

-- |Extend renaming with extra variable.
ext :: (n -> m) -> (S n -> S m)
ext _ FZ = FZ
ext r (FS n) = FS (r n)

-- |Apply renaming to term.
ren :: (n -> m) -> (Term c n -> Term c m)
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
ren _ (Chan n)            = Chan n

-- |Extend sub1itution with extra variable.
exts :: (n -> Term c m) -> (S n -> Term c (S m))
exts _ FZ = Var FZ
exts r (FS n) = FS <$> r n

-- |Apply sub1itution to term.
sub :: (n -> Term c m) -> (Term c n -> Term c m)
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
sub _ (Chan n)            = Chan n

-- |Sub1itute for top-most binding.
sub1 :: Term c (S n) -> Term c n -> Term c n
sub1 x y = sub r x
  where
    r FZ     = y
    r (FS z) = Var z

-- |Check if a term is a value.
isValue :: Term c n -> Bool
isValue (Var _)    = True
isValue (Lam x)    = isValue x
isValue TT         = True
isValue (Pair x y) = isValue x && isValue y
isValue (Inl x)    = isValue x
isValue (Inr x)    = isValue x
isValue _          = False

-- |Reduce a term by a single step, or fail if no more steps can be taken.
stepTerm :: Term c n -> Maybe (Term c n)
stepTerm (App (Lam x) y _)
  | isValue y = Just $ sub1 x y
stepTerm (LetTT TT x) = Just $ x
stepTerm (LetPair (Pair x y) z _ _)
  | isValue x && isValue y = Just $ sub1 (sub1 z (ren FS x)) y
stepTerm (Case (Inl x) y _ _ _)
  | isValue x = Just $ sub1 y x
stepTerm (Case (Inr x) _ z _ _)
  | isValue x = Just $ sub1 z x
stepTerm (Try x y z _)
  | isValue x = Just $ sub1 y z
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



-- |Returns the channel on which this thread is blocked.
isBlocked :: Term c n -> Maybe c
isBlocked (Send _msg (Chan chan) _msgTy) = Just chan
isBlocked (Recv (Chan chan))             = Just chan
isBlocked (Close (Chan chan))            = Just chan
isBlocked (Cancel (Chan chan) _s)        = Just chan
isBlocked _                              = Nothing

-- |Returns whether or not a thread is forking.
isForking :: Term c n -> Bool
isForking (Fork _body) = True
isForking _            = False

-- |Insert a thread into the thread pool.
insertThread :: (Ord c) => Thread c -> Threads c -> Threads c
insertThread thread@(Thread _ tm) Threads{..}
  | isForking tm              = Threads{ forking = thread : forking, .. }
  | Just chan <- isBlocked tm = Threads{ blocked = insert chan thread blocked, .. }
  | isValue   tm              = Threads{ values  = thread : values, .. }
  | otherwise                 = Threads{ busy    = thread : busy, .. }
  where
    insert :: (Ord c) => c -> Thread c -> [(c, Blocked c)] -> [(c, Blocked c)]
    insert chan thread [] = [(chan, Blocked thread)]
    insert chan thread buffer@(entry@(chan', _) : rest) =
      case compare chan chan' of
        LT -> (chan, Blocked thread) : buffer
        EQ -> bimap id (merge thread) entry : rest
          where
            merge :: Thread c -> Blocked c -> Blocked c
            merge thread1 (Blocked thread2) = Ready thread1 thread2
            merge _       (Ready _ _ )      = error "non-binary session"
        GT -> entry : insert chan thread rest
