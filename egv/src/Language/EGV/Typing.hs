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
module Language.EGV.Typing where



import Data.Coolean
import Language.EGV.Base



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
