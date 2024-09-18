module Data.C.Array

import Data.C.Deref
import Data.C.Integer
import Data.C.SizeOf
import Data.Linear.Ref1
import Data.Vect

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

||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
||| type `a`.
|||
||| Reading from and writing to such an array is O(1) and runs in `IO`.
|||
||| See `CArray` for a pure version of mutable C arrays using linear types.
||| See `CArrayIO` for a version of mutable C arrays usable in IO.
|||
||| Note : In typical use cases, the memory allocated for a C array must
|||        be manually released with a call to `free` unless it is part
|||        of a larger structure `Struct` or managed by an external library.
export
record CArray' (t : RTag) (n : Nat) (a : Type) where
  constructor CA
  ptr : AnyPtr

||| Convenience alias for `CArray' RPure`
public export
0 CArray : Nat -> Type -> Type
CArray = CArray' RPure

||| Convenience alias for `CArray' RIO`
public export
0 CArrayIO : Nat -> Type -> Type
CArrayIO = CArray' RIO

public export
InIO (CArray' RIO n a) where

public export %inline
{n : Nat} -> SizeOf a => SizeOf (CArray' t n a) where
  sizeof_ = cast n * sizeof a

export %inline
unsafeUnwrap : CArray' t n a -> AnyPtr
unsafeUnwrap = ptr

export %inline
unsafeWrap : AnyPtr -> CArray' t n a
unsafeWrap = CA

public export
0 IOBox : Type -> Type
IOBox = CArrayIO 1

public export
0 Box : Type -> Type
Box = CArray 1

parameters {auto has : HasIO io}

  ||| Allocates a new C-pointer of `sizeof a * n` bytes.
  export %inline
  malloc : (0 a : Type) -> SizeOf a => (n : Nat) -> io (CArrayIO n a)
  malloc a n = primIO $ MkIORes (CA $ prim__malloc (cast $ n * sizeof a))

  ||| Like `malloc` but resets all allocated bytes to zero.
  export %inline
  calloc : (0 a : Type) -> SizeOf a => (n : Nat) -> io (CArrayIO n a)
  calloc a n =
    primIO $ MkIORes (CA $ prim__calloc (cast n) (cast $ sizeof a))

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
  free : CArrayIO n a -> io ()
  free (CA p) = primIO $ prim__free p

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------

||| Allocates a new C-pointer of `sizeof a * n` bytes.
export %inline
malloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> (1 t : T1 rs)
  -> A1 rs (CArray n a)
malloc1 a n t =
  let p := prim__malloc (cast $ n * sizeof a)
   in A (CA p) (unsafeBind t)

||| Like `malloc1` but resets all allocated bytes to zero.
export %inline
calloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> (1 t : T1 rs)
  -> A1 rs (CArray n a)
calloc1 a n t =
  let p := prim__calloc (cast n) (cast $ sizeof a)
   in A (CA p) (unsafeBind t)

||| Frees the memory allocated for a C pointer and removes it from the
||| resources bound to the linear token.
export %inline
free1 : (r : CArray n a) -> (0 p : Res r rs) => C1' rs (Drop rs p)
free1 r t =
  let MkIORes _ _ := prim__free r.ptr %MkWorld
   in unsafeRelease p t

||| Extracts the first value stored in a C pointer.
export %inline
unbox : Deref a => (r : CArray' t (S n) a) -> (0 p : Res r rs) => F1 rs a
unbox r t = let MkIORes v _ := toPrim (deref r.ptr) %MkWorld in v # t

parameters {0 a      : Type}
           {0 n      : Nat}
           {0 rs     : Resources}
           {auto so  : SizeOf a}
           (r        : CArray' t n a)
           {auto 0 p : Res r rs}

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  get : Deref a => Fin n -> F1 rs a
  get x = ffi $ toPrim (deref $ prim__inc_ptr r.ptr $ cast $ cast x * sizeof a)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => F1 rs a
  getIx m = get (ixToFin x)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => F1 rs a
  getNat m = get (natToFinLT m)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  set : SetPtr a => Fin n -> a -> F1' rs
  set x v = ffi $ toPrim (setPtr (prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) v)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setIx : SetPtr a => (0 m : Nat) -> (x : Ix (S m) n) => a -> F1' rs
  setIx m = set (ixToFin x)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setNat : SetPtr a => (m : Nat) -> (0 lt : LT m n) => a -> F1' rs
  setNat m = set (natToFinLT m)

  writeVect1 : SetPtr a => Vect k a -> Ix k n => F1' rs
  writeVect1           []        t = () # t
  writeVect1 {k = S m} (x :: xs) t =
    let _ # t := setIx m x t
     in writeVect1 xs t

  readVect1 :
       {auto de : Deref a}
    -> Vect m a
    -> (k : Nat)
    -> {auto 0 lt : LTE k n}
    -> F1 rs (Vect (m+k) a)
  readVect1 vs 0 t =
    rewrite plusCommutative m 0 in vs # t
  readVect1 vs (S x) t =
    let v # t := getNat x {lt} t
     in rewrite sym (plusSuccRightSucc m x) in readVect1 (v::vs) x t

  ||| Writes the values from a vector to a C pointer
  export %inline
  writeVect : SetPtr a => Vect n a -> F1' rs
  writeVect as = writeVect1 as

||| Reads the values from a C pointer into a vector.
export %inline
readVect :
     {n : _}
  -> {auto sz : SizeOf a}
  -> {auto de : Deref a}
  -> (r : CArray n a)
  -> {auto 0 p : Res r rs}
  -> F1 rs (Vect n a)
readVect r = readVect1 r [] n

export
withCArray : SizeOf a => (n : Nat) -> (f : (r : CArray n a) -> F1 [r] b) -> b
withCArray n f =
  run1 $ \t =>
    let A r t := malloc1 a n t
        v # t := f r t
        _ # t := free1 r t
     in v # t

-- namespace Immutable
--
--   ||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
--   ||| type `a`.
--   |||
--   ||| Reading from such an array is O(1) and can be done in pure functions.
--   |||
--   ||| See `CArrayIO` for a version of mutable C arrays running in `IO`.
--   ||| See `CArray` for an mutable wrapper to be used in pure (linear) code.
--   |||
--   ||| Note : In typical use cases, the memory allocated for a C array must
--   |||        be manually released with a call to `IO.free` unless it is part
--   |||        of a larger structure or managed by an external library.
--   export
--   record IArray (n : Nat) (a : Type) where
--     constructor IA
--     ptr : AnyPtr
--
--   export %inline
--   unsafeWrap : AnyPtr -> IArray n a
--   unsafeWrap = IA
--
--   parameters {0 a      : Type}
--              {0 n      : Nat}
--              {auto so  : SizeOf a}
--              (r        : IArray n a)
--
--     export %inline
--     get : Deref a => Fin n -> a
--     get x =
--       let MkIORes v _ := toPrim (deref $ prim__inc_ptr r.ptr $ cast $ cast x * sizeof a) %MkWorld
--        in v
--
--     export %inline
--     getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => a
--     getIx m = get (ixToFin x)
--
--     export %inline
--     getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => a
--     getNat m = get (natToFinLT m)
--
--     ||| Frees the memory allocated for the given C-array.
--     export %inline
--     free : IArray n a -> IO ()
--     free (IA p) = fromPrim $ prim__free p
