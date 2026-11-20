module Data.C.Array8

import Control.Monad.Resource
import Data.C.Integer
import Data.C.Array
import Data.Linear.ELift1
import Data.Vect

import public Data.Fin
import public Data.Linear.Token
import public Data.Array.Index

import Syntax.T1

%default total

export %foreign "scheme,chez:(lambda (x y z) (foreign-set! 'unsigned-8 x y z))"
prim__setbits8 : AnyPtr -> Integer -> Bits8 -> PrimIO ()

export %foreign "scheme,chez:(lambda (x y) (foreign-ref 'unsigned-8 x y))"
prim__getbits8 : AnyPtr -> Integer -> Bits8

--------------------------------------------------------------------------------
-- Immutable API
--------------------------------------------------------------------------------

export
record CIArray8 (n : Nat) where
  constructor IA
  ptr : AnyPtr

export %inline
at : CIArray8 n -> Fin n -> Bits8
at r x = prim__getbits8 r.ptr (cast $ finToNat x)

export %inline
ix : CIArray8 n -> (0 m : Nat) -> (x : Ix (S m) n) => Bits8
ix r m = at r (ixToFin x)

export %inline
atNat : CIArray8 n -> (m : Nat) -> (0 lt : LT m n) => Bits8
atNat r m = at r (natToFinLT m)

foldrI : (m : Nat) -> (0 _ : LTE m n) => (Bits8 -> b -> b) -> b -> CIArray8 n -> b
foldrI 0     _ x r = x
foldrI (S k) f x r = foldrI k f (f (atNat r k) x) r

foldrKV_ :
     (m : Nat)
  -> {auto 0 prf : LTE m n}
  -> (Fin n -> Bits8 -> b -> b)
  -> b
  -> CIArray8 n
  -> b
foldrKV_ 0     _ x r = x
foldrKV_ (S k) f x r =
  let fin := natToFinLT k @{prf} in foldrKV_ k f (f fin (at r fin) x) r

foldlI : (m : Nat) -> (x : Ix m n) => (b -> Bits8 -> b) -> b -> CIArray8 n -> b
foldlI 0     _ v r = v
foldlI (S k) f v r = foldlI k f (f v (ix r k)) r

foldlKV_ :
     (m : Nat)
  -> {auto x : Ix m n}
  -> (Fin n -> b -> Bits8 -> b)
  -> b
  -> CIArray8 n
  -> b
foldlKV_ 0     _ v r = v
foldlKV_ (S k) f v r =
  let fin := ixToFin x in foldlKV_ k f (f fin v (at r fin)) r

ontoVect :
     (r : CIArray8 n)
  -> Vect m Bits8
  -> (k : Nat)
  -> {auto 0 lt : LTE k n}
  -> Vect (m+k) Bits8
ontoVect r vs 0     = rewrite plusCommutative m 0 in vs
ontoVect r vs (S x) =
  let v := atNat r x {lt}
   in rewrite sym (plusSuccRightSucc m x) in ontoVect r (v::vs) x

||| Reads the values from a C pointer into a vector.
export %inline
toVect : {n : _} -> (r : CIArray8 n) -> Vect n Bits8
toVect r = ontoVect r [] n

||| Right fold over the values of an array plus their indices.
export %inline
foldrKV : {n : _} -> (Fin n -> Bits8 -> b -> b) -> b -> CIArray8 n -> b
foldrKV = foldrKV_ n

||| Right fold over the values of an array
export %inline
foldr : {n : _} -> (Bits8 -> b -> b) -> b -> CIArray8 n -> b
foldr = foldrI n

||| Left fold over the values of an array plus their indices.
export %inline
foldlKV : {n : _} -> (Fin n -> b -> Bits8 -> b) -> b -> CIArray8 n -> b
foldlKV = foldlKV_ n

||| Left fold over the values of an array
export %inline
foldl : {n : _} -> (b -> Bits8 -> b) -> b -> CIArray8 n -> b
foldl = foldlI n

--------------------------------------------------------------------------------
-- Mutable C Arrays
--------------------------------------------------------------------------------

||| A wrapped pointer to a C-array holding `n` unsigned 8-bit
||| values (C-primitive).
|||
||| Reading from and writing to such an array is O(1) and runs in `IO`.
|||
||| See `CArray8` for a pure version of mutable C arrays using linear types.
||| See `CArray8S` for a pure versoin of mutable C arrays using linear types (overwritting/scrubbing before freeing).
||| See `CArray8IO` for a version of mutable C arrays usable in IO.
||| See `CArray8IOS` for a version of the mutable C arrays usuable in IO (overwritting/scrubbing before freeing)
|||
||| Note : In typical use cases, the memory allocated for a C array must
|||        be manually released with a call to `free` unless it is part
|||        of a larger structure `Struct` or managed by an external library.
export
record PrimCArray8 (s : Type) (b : Bool) (n : Nat) where
  constructor PCA8
  ptr : AnyPtr

||| Convenience alias for `PrimCArray8 s False n a`
public export
0 CArray8 : (s : Type) -> (n : Nat) -> Type
CArray8 s n = PrimCArray8 s False n

||| Convenience alias for `PrimCArray8 s True n a`
public export
0 CArray8S : (s : Type) -> (n : Nat) -> Type
CArray8S s n = PrimCArray8 s True n

||| Convenience alias for `PrimCArray8 World False n a`
public export
0 CArray8IO : (n : Nat) -> Type
CArray8IO n = PrimCArray8 World False n

||| Convenience alias for `PrimCArray8 World True n a`
public export
0 CArray8IOS : (n : Nat) -> Type
CArray8IOS n = PrimCArray8 World True n

export %inline
unsafeUnwrap : CArray8 s n -> AnyPtr
unsafeUnwrap = ptr

export %inline
unsafeWrap : AnyPtr -> CArray8 s n
unsafeWrap = PCA8

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------

||| Allocates a new C-pointer of `sizeof a * n` bytes.
export %inline
malloc1 : (n : Nat) -> F1 s (PrimCArray8 s b n)
malloc1 n t =
  let p := prim__malloc (cast n)
   in PCA8 p # t

||| Like `malloc1` but resets all allocated bytes to zero.
export %inline
calloc1 : (n : Nat) -> F1 s (PrimCArray8 s b n)
calloc1 n t =
  let p := prim__calloc (cast n) 1
   in PCA8 p # t

||| Frees the memory allocated for a C array without
||| overwriting its contents.
export %inline
free1 : (r : CArray8 s n) -> F1' s
free1 r = ffi (prim__free r.ptr)

||| Overwrites the contents of a C array before freeing
||| the underlying memory.
export %inline
frees1 : {n : Nat} -> (r : CArray8S s n) -> F1' s
frees1 {n} r t =
  let () # t := ffi (prim__scrub r.ptr (cast n)) t
   in ffi (prim__free r.ptr) t

parameters {0 n      : Nat}
           (r        : PrimCArray8 s b n)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  get : Fin n -> F1 s Bits8
  get x t = prim__getbits8 r.ptr (cast $ finToNat x) # t

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getIx : (0 m : Nat) -> (x : Ix (S m) n) => F1 s Bits8
  getIx m = get (ixToFin x)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getNat : (m : Nat) -> (0 lt : LT m n) => F1 s Bits8
  getNat m = get (natToFinLT m)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  set : Fin n -> Bits8 -> F1' s
  set x v = ffi $ prim__setbits8 r.ptr (cast $ finToNat x) v

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setIx : (0 m : Nat) -> (x : Ix (S m) n) => Bits8 -> F1' s
  setIx m = set (ixToFin x)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setNat : (m : Nat) -> (0 lt : LT m n) => Bits8 -> F1' s
  setNat m = set (natToFinLT m)

  writeVect1 : Vect k Bits8 -> Ix k n => F1' s
  writeVect1           []        t = () # t
  writeVect1 {k = S m} (x :: xs) t =
    let _ # t := Array8.setIx m x t
     in writeVect1 xs t

  ||| Writes the values from a vector to a C pointer
  export %inline
  writeVect : Vect n Bits8 -> F1' s
  writeVect as = writeVect1 as

  ||| Temporarily wraps the mutable array in an immutable wrapper and
  ||| run a computation with that.
  |||
  ||| This is safe, because the pure function cannot possibly share the
  ||| immutable array by storing it in a mutable reference. It is
  ||| referentially transparent, because we call it from a linear context.
  export %inline
  withIArray : (CIArray8 n -> c) -> F1 s c
  withIArray f t = f (IA r.ptr) # t

||| Writes the values from a list to a C pointer
export %inline
writeList : (as : List Bits8) -> PrimCArray8 s b (length as) -> F1' s
writeList as r = writeVect r (fromList as)

||| Allocates a C array, runs a computation, and releases
||| the array using the supplied cleanup operation.
|||
||| The Boolean index is erased because cleanup behavior
||| is supplied explicitly rather than selected by inspecting b.
export
withPrimCArray8 :
     {0 b : Bool}
  -> (n : Nat)
  -> (release : forall s . PrimCArray8 s b n -> F1' s)
  -> (f : forall s . PrimCArray8 s b n -> F1 s c)
  -> c
withPrimCArray8 n release f =
  run1 $ \t =>
    let r  # t := Array8.malloc1 n t
        v  # t := f r t
        () # t := release r t
     in v # t

||| Allocates a C array, runs a computation, and frees
||| the underlying memory afterward.
export
withCArray :
     (n : Nat)
  -> (f : forall s . CArray8 s n -> F1 s b)
  -> b
withCArray n f =
  withPrimCArray8 n free1 f

||| Allocates a C array, runs a computation, and securely
||| scrubs the underlying memory before freeing it.
export
withCArrayS :
     (n : Nat)
  -> (f : forall s . CArray8S s n -> F1 s b)
  -> b
withCArrayS n f =
  withPrimCArray8 n frees1 f

--------------------------------------------------------------------------------
-- Lift1 API
--------------------------------------------------------------------------------

parameters {auto has : Lift1 s f}

  ||| Allocates a new C-pointer of `sizeof a * n` bytes.
  export %inline
  malloc : (n : Nat) -> f (PrimCArray8 s b n)
  malloc n = lift1 (malloc1 n)

  ||| Like `malloc` but resets all allocated bytes to zero.
  export %inline
  calloc : (n : Nat) -> f (PrimCArray8 s b n)
  calloc n = lift1 (calloc1 n)

  ||| Frees a C array without scrubbing its contents.
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
  free : CArray8 s n -> f ()
  free arr = lift1 (free1 arr)

  ||| Scrubs and frees a C array.
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
  frees : {n : Nat} -> CArray8S s n -> f ()
  frees arr = lift1 (frees1 arr)

  export %inline
  fromList : (as : List Bits8) -> f (PrimCArray8 s b (length as))
  fromList as = Prelude.do
    arr <- Array8.malloc (length as)
    lift1 $ writeList as arr
    pure arr

--------------------------------------------------------------------------------
-- Resource
--------------------------------------------------------------------------------

export %inline
ELift1 s f => Resource f (CArray8 s n) where
  cleanup arr = lift1 (free1 arr)

export %inline
{n : Nat} -> ELift1 s f => Resource f (CArray8S s n) where
  cleanup arr = lift1 (frees1 arr)
