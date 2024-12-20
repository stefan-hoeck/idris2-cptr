module Data.C.Array

import Data.C.Deref
import Data.C.Integer
import Data.C.SizeOf
import Data.Vect

import public Data.Fin
import public Data.Linear.Token
import public Data.Array.Index

import Syntax.T1

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

export %foreign "C:cptr_malloc, cptr-idris"
                "scheme,chez:(lambda (x) (if (= 0 x) 0 (foreign-alloc x)))"
prim__malloc : (size : Bits32) -> AnyPtr

export %foreign "C:cptr_calloc, cptr-idris"
prim__calloc : (n, size : Bits32) -> AnyPtr

export %foreign "C:cptr_free, cptr-idris"
                "scheme,chez:(lambda (x) (if (= 0 x) '() (foreign-free x)))"
prim__free : AnyPtr -> PrimIO ()

export %foreign "C:cptr_inc_ptr, cptr-idris"
                "scheme,chez:(lambda (p x y) (+ p (* x y)))"
prim__inc_ptr : AnyPtr -> Bits32 -> Bits32 -> AnyPtr

--------------------------------------------------------------------------------
-- Immutable API
--------------------------------------------------------------------------------

||| A wrapped pointer to a C-array holding `n` values of (C-primitive)
||| type `a`.
|||
||| Reading from such an array is O(1) and can be done in pure functions.
|||
||| See `CArrayIO` for a version of mutable C arrays running in `IO`.
||| See `CArray` for an mutable wrapper to be used in pure (linear) code.
|||
||| Note : In general, this type is not for prolonged storage in an Idris data
|||        structure (although this is still possible), because it needs to be
|||        eventually freed. A typical use case is to make use of this for
|||        its pure and clean API, but to do so from within `IO` or `F1` by
|||        using `withIArray`.
export
record CIArray (n : Nat) (a : Type) where
  constructor IA
  ptr : AnyPtr

parameters {0 a      : Type}
           {0 n      : Nat}
           {auto so  : SizeOf a}
           {auto dr  : Deref a}

  export %inline
  at : CIArray n a -> Fin n -> a
  at r x =
    let MkIORes v _ := toPrim (deref $ prim__inc_ptr r.ptr (sizeof a) (cast $ finToNat x)) %MkWorld
     in v

  export %inline
  ix : CIArray n a -> (0 m : Nat) -> (x : Ix (S m) n) => a
  ix r m = at r (ixToFin x)

  export %inline
  atNat : CIArray n a -> (m : Nat) -> (0 lt : LT m n) => a
  atNat r m = at r (natToFinLT m)

  foldrI : (m : Nat) -> (0 _ : LTE m n) => (a -> b -> b) -> b -> CIArray n a -> b
  foldrI 0     _ x r = x
  foldrI (S k) f x r = foldrI k f (f (atNat r k) x) r

  foldrKV_ :
       (m : Nat)
    -> {auto 0 prf : LTE m n}
    -> (Fin n -> a -> b -> b)
    -> b
    -> CIArray n a
    -> b
  foldrKV_ 0     _ x r = x
  foldrKV_ (S k) f x r =
    let fin := natToFinLT k @{prf} in foldrKV_ k f (f fin (at r fin) x) r

  foldlI : (m : Nat) -> (x : Ix m n) => (b -> a -> b) -> b -> CIArray n a -> b
  foldlI 0     _ v r = v
  foldlI (S k) f v r = foldlI k f (f v (ix r k)) r

  foldlKV_ :
       (m : Nat)
    -> {auto x : Ix m n}
    -> (Fin n -> b -> a -> b)
    -> b
    -> CIArray n a
    -> b
  foldlKV_ 0     _ v r = v
  foldlKV_ (S k) f v r =
    let fin := ixToFin x in foldlKV_ k f (f fin v (at r fin)) r

  ontoVect :
       (r : CIArray n a)
    -> Vect m a
    -> (k : Nat)
    -> {auto 0 lt : LTE k n}
    -> Vect (m+k) a
  ontoVect r vs 0     = rewrite plusCommutative m 0 in vs
  ontoVect r vs (S x) =
    let v := atNat r x {lt}
     in rewrite sym (plusSuccRightSucc m x) in ontoVect r (v::vs) x

parameters {n : Nat}
           {auto sz : SizeOf a}
           {auto de : Deref a}

  ||| Reads the values from a C pointer into a vector.
  export %inline
  toVect : (r : CIArray n a) -> Vect n a
  toVect r = ontoVect r [] n

  ||| Right fold over the values of an array plus their indices.
  export %inline
  foldrKV : (Fin n -> a -> b -> b) -> b -> CIArray n a -> b
  foldrKV = foldrKV_ n

  ||| Right fold over the values of an array
  export %inline
  foldr : (a -> b -> b) -> b -> CIArray n a -> b
  foldr = foldrI n

  ||| Left fold over the values of an array plus their indices.
  export %inline
  foldlKV : (Fin n -> b -> a -> b) -> b -> CIArray n a -> b
  foldlKV = foldlKV_ n

  ||| Left fold over the values of an array
  export %inline
  foldl : (b -> a -> b) -> b -> CIArray n a -> b
  foldl = foldlI n

--------------------------------------------------------------------------------
-- IO-API
--------------------------------------------------------------------------------

||| Allocates a pointer of the given size and uses it for running
||| the given computation. The pointer is freed afterwards.
export %inline
withPtr : HasIO io => Bits32 -> (AnyPtr -> io a) -> io a
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
  malloc a n = primIO $ MkIORes (CA $ prim__malloc (cast n * sizeof a))

  ||| Like `malloc` but resets all allocated bytes to zero.
  export %inline
  calloc : (0 a : Type) -> SizeOf a => (n : Nat) -> io (CArrayIO n a)
  calloc a n =
    primIO $ MkIORes (CA $ prim__calloc (cast n) (sizeof a))

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
  let p := prim__malloc (cast n * sizeof a)
   in CA p # unsafeBind t

||| Like `malloc1` but resets all allocated bytes to zero.
export %inline
calloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> (1 t : T1 rs)
  -> A1 rs (CArray n a)
calloc1 a n t =
  let p := prim__calloc (cast n) (sizeof a)
   in CA p # unsafeBind t

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
  get x = ffi $ toPrim (deref $ prim__inc_ptr r.ptr (cast $ finToNat x) (sizeof a))

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
  set x v = ffi $ toPrim (setPtr (prim__inc_ptr r.ptr (cast $ finToNat x) (sizeof a)) v)

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

  ||| Writes the values from a vector to a C pointer
  export %inline
  writeVect : SetPtr a => Vect n a -> F1' rs
  writeVect as = writeVect1 as

  ||| Temporarily wraps the mutable array in an immutable wrapper and
  ||| run a computation with that.
  |||
  ||| This is safe, because the pure function cannot possibly share the
  ||| immutable array by storing it in a mutable reference. It is
  ||| referentially transparent, because we call it from a linear context.
  export %inline
  withIArray : (CIArray n a -> b) -> F1 rs b
  withIArray f t = f (IA r.ptr) # t

||| Writes the values from a list to a C pointer
export %inline
writeList :
     {auto so  : SizeOf a}
  -> {auto sp  : SetPtr a}
  -> (as       : List a)
  -> (r        : CArray' t (length as) a)
  -> {auto 0 p : Res r rs}
  -> F1' rs
writeList as r = writeVect r (fromList as)

export
withCArray : SizeOf a => (n : Nat) -> (f : (r : CArray n a) -> F1 [r] b) -> b
withCArray n f =
  run1 $ \t =>
    let r # t := malloc1 a n t
        v # t := f r t
        _ # t := free1 r t
     in v # t

export %inline
fromListIO :
     {auto has : HasIO io}
  -> {auto sz  : SizeOf a}
  -> {auto sp  : SetPtr a}
  -> (as : List a)
  -> io (CArrayIO (length as) a)
fromListIO as = Prelude.do
  arr <- malloc a (length as)
  runIO $ writeList as arr
  pure arr
