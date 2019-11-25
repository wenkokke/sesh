{-# OPTIONS -fno-warn-name-shadowing   #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Language.EGV where



import           Control.Enumerable
import           Data.Bifunctor
import           Data.Coolean
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Typeable      (Typeable)



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




-- * Type checking

-- |Contains type and session type environment.
data TCS c n = (Ord c, Ord n) => TCS
  { typeEnv :: n -> Type
  , sessEnv :: c -> Sess
  }


-- |Empty type checking state.
emptyTCS :: TCS Z Z
emptyTCS = TCS absurd absurd


-- |Check the type of a term.
checkType :: forall c n. TCS c n -> Type -> Term c n -> Cool
checkType TCS{..} ty (Var n) = toCool $ ty == typeEnv n
checkType TCS{..} ty (Chan c) = toCool $ ty == Emb (sessEnv c)
checkType TCS{..} (argTy :-> ty) (Lam body) =
  checkType tcs' ty body
  where
    typeEnv' :: S n -> Type
    typeEnv' FZ = argTy
    typeEnv' (FS n) = typeEnv n

    tcs' = TCS { typeEnv = typeEnv', .. }

checkType tcs ty (App fun arg argTy) = checkType tcs (argTy :-> ty) fun &&& checkType tcs argTy arg
checkType _ Unit TT = true
checkType tcs ty (LetTT tt body) = checkType tcs Unit tt &&& checkType tcs ty body
checkType tcs (inlTy :+ _inrTy) (Inl tm) = checkType tcs inlTy tm
checkType tcs (_inlTy :+ inrTy) (Inr tm) = checkType tcs inrTy tm
checkType tcs@TCS{..} ty (Case sum inlBody inrBody inlTy inrTy) =
  checkType tcs (inlTy :+ inrTy) sum &&& checkType inlTcs ty inlBody &&& checkType inrTcs ty inrBody
  where
    inlTypeEnv :: S n -> Type
    inlTypeEnv FZ = inlTy
    inlTypeEnv (FS n) = typeEnv n

    inlTcs = TCS { typeEnv = inlTypeEnv, .. }

    inrTypeEnv :: S n -> Type
    inrTypeEnv FZ = inrTy
    inrTypeEnv (FS n) = typeEnv n

    inrTcs = TCS { typeEnv = inrTypeEnv, .. }

checkType tcs (Emb s) (Fork tm) = checkType tcs (Emb (dual s) :-> Unit) tm
checkType tcs (Emb s) (Send msg chan msgTy) = checkType tcs msgTy msg &&& checkType tcs (Emb (msgTy :! s)) chan
checkType tcs (ty :* Emb s) (Recv chan) = checkType tcs (Emb (ty :? s)) chan
checkType tcs Unit (Close chan) = checkType tcs (Emb End) chan
checkType tcs Unit (Cancel chan s) = checkType tcs (Emb s) chan
checkType tcs@TCS{..} ty (Try tm body exn tmTy) =
  checkType tcs tmTy tm &&& checkType tcs' ty body &&& checkType tcs ty exn
  where
    typeEnv' :: S n -> Type
    typeEnv' FZ = tmTy
    typeEnv' (FS n) = typeEnv n

    tcs' = TCS { typeEnv = typeEnv', .. }

checkType _ _ Raise = true
checkType _ _ _ = false



-- * Usage checking

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

checkTypeAndUsage :: Type -> Term Z Z -> Cool
checkTypeAndUsage ty tm = checkType emptyTCS ty tm !&& checkUsage tm



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


-- * Configurations

data Conf c
  = New (Conf (S c))
  | Conf (Threads c)

data Threads c = Threads
  { busy    :: [Busy c]         -- ^ Busy threads
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

type Busy c = Thread c

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

-- |Merge two blocked threads.
insertBlocked :: Thread c -> Blocked c -> Blocked c
insertBlocked thread1 (Blocked thread2) = Ready thread1 thread2
insertBlocked _       (Ready _ _ )      = error "broke *binary* constraint on session types"

-- |Insert a blocked thread into the buffer of blocked threads.
insertBuffer :: (Ord c) => c -> Thread c -> [(c, Blocked c)] -> [(c, Blocked c)]
insertBuffer chan thread [] = [(chan, Blocked thread)]
insertBuffer chan thread buffer@(entry@(chan', _) : rest) = case compare chan chan' of
  LT -> (chan, Blocked thread) : buffer
  EQ -> bimap id (insertBlocked thread) entry : rest
  GT -> entry : insertBuffer chan thread rest

-- |Insert a thread into the thread pool.
insertThread :: (Ord c) => Thread c -> Threads c -> Threads c
insertThread thread@(Thread _ tm) Threads{..}
  | isForking tm              = Threads{ forking = thread : forking, .. }
  | Just chan <- isBlocked tm = Threads{ blocked = insertBuffer chan thread blocked, .. }
  | isValue   tm              = Threads{ values  = thread : values, .. }
  | otherwise                 = Threads{ busy    = thread : busy, .. }
