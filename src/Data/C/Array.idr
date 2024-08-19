module Data.C.Array

import Data.C.Deref
import Data.C.Integer
import Data.C.SizeOf
import Data.Linear.Ref1

import public Data.Fin
import public Data.Array.Index

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "C:idris2_malloc, libidris2_support, idris_memory.h"
prim__malloc : (size : SizeT) -> PrimIO AnyPtr

%foreign "C:idris2_free, libidris2_support, idris_memory.h"
prim__free : AnyPtr -> PrimIO ()

%foreign "C:cptr_inc_ptr, cptr-idris"
prim__inc_ptr : AnyPtr -> SizeT -> AnyPtr

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

export
record CArray (n : Nat) (a : Type) where
  constructor CA
  ptr : AnyPtr

export %inline
malloc : (0 a : Type) -> SizeOf a => (n : Nat) -> IO (CArray n a)
malloc a n = believe_me $ fromPrim $ prim__malloc (cast $ n * sizeof a)

export %inline
free : CArray n a -> IO ()
free (CA p) = fromPrim $ prim__free p

export %inline
getIO : Deref a => SizeOf a => CArray n a -> Fin n -> IO a
getIO (CA p) x = deref (prim__inc_ptr p $ cast $ cast x * sizeof a)

export %inline
setIO : SetPtr a => SizeOf a => CArray n a -> Fin n -> a -> IO ()
setIO (CA p) x v = setPtr (prim__inc_ptr p $ cast $ cast x * sizeof a) v

public export %inline
{n : Nat} -> SizeOf a => SizeOf (CArray n a) where
  sizeof_ = cast n * sizeof a

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------


export %inline
malloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> (1 t : T1 rs)
  -> A1 rs (CArray n a)
malloc1 a n t =
  let MkIORes p _ := prim__malloc (cast $ n * sizeof a) %MkWorld
   in A (CA p) (unsafeBind t)

export %inline
free1 : (r : CArray n a) -> (0 p : Res r rs) => C1' rs (Drop rs p)
free1 r t =
  let MkIORes _ _ := toPrim (free r) %MkWorld
   in unsafeRelease p t

parameters {0 a      : Type}
           {0 n      : Nat}
           {0 rs     : Resources}
           {auto so  : SizeOf a}
           (r        : CArray n a)
           {auto 0 p : Res r rs}

  export %inline
  get : Deref a => Fin n -> F1 rs a
  get x t =
    let MkIORes v _ := toPrim (deref $ prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) %MkWorld
     in v # t

  export %inline
  getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => F1 rs a
  getIx m = get (ixToFin x)

  export %inline
  getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => F1 rs a
  getNat m = get (natToFinLT m)

  export %inline
  set : SetPtr a => Fin n -> a -> F1' rs
  set x v t =
    let MkIORes v _ := toPrim (setPtr (prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) v) %MkWorld
     in t

  export %inline
  setIx : SetPtr a => (0 m : Nat) -> (x : Ix (S m) n) => a -> F1' rs
  setIx m = set (ixToFin x)

  export %inline
  setNat : SetPtr a => (m : Nat) -> (0 lt : LT m n) => a -> F1' rs
  setNat m = set (natToFinLT m)
