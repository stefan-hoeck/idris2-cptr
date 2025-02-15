module Data.C.Array8

import Data.C.Integer
import Data.C.Array
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
-- IO-API
--------------------------------------------------------------------------------

export
record CArray8 (s : Type) (n : Nat) where
  constructor CA
  ptr : AnyPtr

||| Convenience alias for `CArray8' RIO`
public export
0 CArray8IO : Nat -> Type
CArray8IO = CArray8 World

export %inline
unsafeUnwrap : CArray8 s n -> AnyPtr
unsafeUnwrap = ptr

export %inline
unsafeWrap : AnyPtr -> CArray8 s n
unsafeWrap = CA

parameters {auto has : HasIO io}

  ||| Allocates a new C-pointer of `sizeof a * n` bytes.
  export %inline
  malloc : (n : Nat) -> io (CArray8IO n)
  malloc n = primIO $ MkIORes (CA $ prim__malloc (cast n))

  ||| Like `malloc` but resets all allocated bytes to zero.
  export %inline
  calloc : (n : Nat) -> io (CArray8IO n)
  calloc n =
    primIO $ MkIORes (CA $ prim__calloc (cast n) 1)

  export %inline
  free : CArray8IO n -> io ()
  free (CA p) = primIO $ prim__free p

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------

||| Allocates a new C-pointer of `sizeof a * n` bytes.
export %inline
malloc1 : (n : Nat) -> F1 s (CArray8 s n)
malloc1 n t =
  let p := prim__malloc (cast n)
   in CA p # t

||| Like `malloc1` but resets all allocated bytes to zero.
export %inline
calloc1 : (n : Nat) -> F1 s (CArray8 s n)
calloc1 n t =
  let p := prim__calloc (cast n) 1
   in CA p # t

||| Frees the memory allocated for a C pointer and removes it from the
||| resources bound to the linear token.
export %inline
free1 : (r : CArray8 s n) -> F1' s
free1 r = ffi (prim__free r.ptr)

parameters {0 n      : Nat}
           (r        : CArray8 s n)

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
  withIArray : (CIArray8 n -> b) -> F1 s b
  withIArray f t = f (IA r.ptr) # t

||| Writes the values from a list to a C pointer
export %inline
writeList : (as : List Bits8) -> CArray8 s (length as) -> F1' s
writeList as r = writeVect r (fromList as)

export
withCArray : (n : Nat) -> (f : forall s . CArray8 s n -> F1 s b) -> b
withCArray n f =
  run1 $ \t =>
    let r # t := Array8.malloc1 n t
        v # t := f r t
        _ # t := Array8.free1 r t
     in v # t

export %inline
fromListIO :
     {auto has : HasIO io}
  -> (as : List Bits8)
  -> io (CArray8IO (length as))
fromListIO as = Prelude.do
  arr <- Array8.malloc (length as)
  runIO $ writeList as arr
  pure arr
