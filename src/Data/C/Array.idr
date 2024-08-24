module Data.C.Array

import Data.C.Deref
import Data.C.Integer
import Data.C.SizeOf
import Data.Linear.Ref1

import public Data.Fin
import public Data.Array.Index

import Syntax.T1

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "C:cptr_malloc, cptr-idris"
prim__malloc : (size : SizeT) -> AnyPtr

%foreign "C:cptr_calloc, cptr-idris"
prim__calloc : (n, size : SizeT) -> AnyPtr

%foreign "C:cptr_free, cptr-idris"
prim__free : AnyPtr -> PrimIO ()

%foreign "C:cptr_inc_ptr, cptr-idris"
prim__inc_ptr : AnyPtr -> SizeT -> AnyPtr

--------------------------------------------------------------------------------
-- IO-API
--------------------------------------------------------------------------------

namespace IO

  export
  record CArrayIO (n : Nat) (a : Type) where
    constructor CAIO
    ptr : AnyPtr

  export %inline
  unsafeUnwrap : CArrayIO n a -> AnyPtr
  unsafeUnwrap = ptr

  export %inline
  unsafeWrap : AnyPtr -> CArrayIO n a
  unsafeWrap = CAIO

  public export
  0 CRefIO : Type -> Type
  CRefIO = CArrayIO 1

  export %inline
  malloc : (0 a : Type) -> SizeOf a => (n : Nat) -> IO (CArrayIO n a)
  malloc a n = fromPrim $ MkIORes (CAIO $ prim__malloc (cast $ n * sizeof a))

  export %inline
  calloc : (0 a : Type) -> SizeOf a => (n : Nat) -> IO (CArrayIO n a)
  calloc a n = fromPrim $ MkIORes (CAIO $ prim__calloc (cast n) (cast $ sizeof a))

  export %inline
  free : CArrayIO n a -> IO ()
  free (CAIO p) = fromPrim $ prim__free p

  export %inline
  unboxIO : Deref a => (r : CArrayIO (S n) a) -> IO a
  unboxIO r = deref r.ptr

  export %inline
  getIO : Deref a => SizeOf a => CArrayIO n a -> Fin n -> IO a
  getIO (CAIO p) x = deref (prim__inc_ptr p $ cast $ cast x * sizeof a)

  export %inline
  setIO : SetPtr a => SizeOf a => CArrayIO n a -> Fin n -> a -> IO ()
  setIO (CAIO p) x v = setPtr (prim__inc_ptr p $ cast $ cast x * sizeof a) v

  public export %inline
  {n : Nat} -> SizeOf a => SizeOf (CArrayIO n a) where
    sizeof_ = cast n * sizeof a

  export
  withCArray : SizeOf a => (n : Nat) -> (f : CArrayIO n a -> IO b) -> IO b
  withCArray n f = do
    arr <- malloc a n
    res <- f arr
    free arr
    pure res

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------

namespace Linear

  export
  record CArray (n : Nat) (a : Type) where
    constructor CA
    ptr : AnyPtr

  export %inline
  unsafeUnwrap : CArray n a -> AnyPtr
  unsafeUnwrap = ptr

  export %inline
  unsafeWrap : AnyPtr -> CArray n a
  unsafeWrap = CA

  export %inline
  malloc :
       (0 a : Type)
    -> {auto so : SizeOf a}
    -> (n : Nat)
    -> (1 t : T1 rs)
    -> A1 rs (CArray n a)
  malloc a n t =
    let p := prim__malloc (cast $ n * sizeof a)
     in A (CA p) (unsafeBind t)

  export %inline
  calloc :
       (0 a : Type)
    -> {auto so : SizeOf a}
    -> (n : Nat)
    -> (1 t : T1 rs)
    -> A1 rs (CArray n a)
  calloc a n t =
    let p := prim__calloc (cast n) (cast $ sizeof a)
     in A (CA p) (unsafeBind t)

  export %inline
  free : (r : CArray n a) -> (0 p : Res r rs) => C1' rs (Drop rs p)
  free r t =
    let MkIORes _ _ := prim__free r.ptr %MkWorld
     in unsafeRelease p t

  export %inline
  unbox : Deref a => (r : CArray (S n) a) -> (0 p : Res r rs) => F1 rs a
  unbox r t = let MkIORes v _ := toPrim (deref r.ptr) %MkWorld in v # t

  parameters {0 a      : Type}
             {0 n      : Nat}
             {0 rs     : Resources}
             {auto so  : SizeOf a}
             (r        : CArray n a)
             {auto 0 p : Res r rs}

    export %inline
    get : Deref a => Fin n -> F1 rs a
    get x = ffi $ toPrim (deref $ prim__inc_ptr r.ptr $ cast $ cast x * sizeof a)

    export %inline
    getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => F1 rs a
    getIx m = get (ixToFin x)

    export %inline
    getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => F1 rs a
    getNat m = get (natToFinLT m)

    export %inline
    set : SetPtr a => Fin n -> a -> F1' rs
    set x v = ffi $ toPrim (setPtr (prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) v)

    export %inline
    setIx : SetPtr a => (0 m : Nat) -> (x : Ix (S m) n) => a -> F1' rs
    setIx m = set (ixToFin x)

    export %inline
    setNat : SetPtr a => (m : Nat) -> (0 lt : LT m n) => a -> F1' rs
    setNat m = set (natToFinLT m)

  export
  withCArray : SizeOf a => (n : Nat) -> (f : (r : CArray n a) -> F1 [r] b) -> b
  withCArray n f =
    run1 $ \t =>
      let A r t := malloc a n t
          v # t := f r t
          _ # t := Linear.free r t
       in v # t
