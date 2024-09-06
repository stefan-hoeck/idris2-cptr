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

export %foreign "C:cptr_malloc, cptr-idris"
prim__malloc : (size : SizeT) -> AnyPtr

export %foreign "C:cptr_calloc, cptr-idris"
prim__calloc : (n, size : SizeT) -> AnyPtr

export %foreign "C:cptr_free, cptr-idris"
prim__free : AnyPtr -> PrimIO ()

export %foreign "C:cptr_inc_ptr, cptr-idris"
prim__inc_ptr : AnyPtr -> SizeT -> AnyPtr

--------------------------------------------------------------------------------
-- IO-API
--------------------------------------------------------------------------------

||| Allocates a pointer of the given size and uses it for running
||| the given computation. The pointer is freed afterwards.
export %inline
withPtr : HasIO io => SizeT -> (AnyPtr -> io a) -> io a
withPtr sz f = Prelude.do
  ptr <- pure $ prim__malloc sz
  res <- f ptr
  primIO $ prim__free ptr
  pure res

namespace IO

  ||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
  ||| type `a`.
  |||
  ||| Reading from and writing to such an array is O(1) and runs in `IO`.
  |||
  ||| See `CArray` for a pure version of mutable C arrays using linear types.
  ||| See `IArray` for an immutable wrapper to be used in pure code.
  |||
  ||| Note : In typical use cases, the memory allocated for a C array must
  |||        be manually released with a call to `IO.free` unless it is part
  |||        of a larger structure `Struct` or managed by an external library.
  |||        If possible, it is advisable to use utility `withCArray` to
  |||        automatically allocate and free an array of the desired size.
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

  ||| Frees the memory allocated for a C-array.
  |||
  ||| Note: Only call this if the C array is no longer used and has been
  |||       allocated via a call to `malloc` or `alloc` (either in C land
  |||       or in Idris). Afterwards, it is no longer safe to use the array
  |||       for reading or writing, nor is it safe to call `free` on it again.
  |||
  |||       For safe resource management, use the linear version of
  |||       C arrays if possible. Otherwise, consider using a safer monad
  |||       than `IO` if possible.
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

  ||| Safely allocates, uses, and frees a new C-array.
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

  ||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
  ||| type `a`.
  |||
  ||| Reading from and writing to such an array is O(1) and must run in a
  ||| pure (but linear) context.
  |||
  ||| See `CArrayIO` for a version of mutable C arrays running in `IO`.
  ||| See `IArray` for an immutable wrapper to be used in pure code.
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

namespace Immutable

  ||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
  ||| type `a`.
  |||
  ||| Reading from such an array is O(1) and can be done in pure functions.
  |||
  ||| See `CArrayIO` for a version of mutable C arrays running in `IO`.
  ||| See `CArray` for an mutable wrapper to be used in pure (linear) code.
  |||
  ||| Note : In typical use cases, the memory allocated for a C array must
  |||        be manually released with a call to `IO.free` unless it is part
  |||        of a larger structure or managed by an external library.
  export
  record IArray (n : Nat) (a : Type) where
    constructor IA
    ptr : AnyPtr

  export %inline
  unsafeWrap : AnyPtr -> IArray n a
  unsafeWrap = IA

  parameters {0 a      : Type}
             {0 n      : Nat}
             {auto so  : SizeOf a}
             (r        : IArray n a)

    export %inline
    get : Deref a => Fin n -> a
    get x =
      let MkIORes v _ := toPrim (deref $ prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) %MkWorld
       in v

    export %inline
    getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => a
    getIx m = get (ixToFin x)

    export %inline
    getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => a
    getNat m = get (natToFinLT m)

    ||| Frees the memory allocated for the given C-array.
    export %inline
    free : IArray n a -> IO ()
    free (IA p) = fromPrim $ prim__free p
