module Data.C.ScrubbedArray

import Control.Monad.Resource
import Data.Buffer
import Data.C.Deref
import Data.C.Integer
import Data.C.SizeOf
import Data.Linear.ELift1
import Data.Vect

import public Data.Fin
import public Data.Linear.Token
import public Data.Array.Index
import public Data.C.Struct

import Syntax.T1

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

export %foreign "C__collect_safe:cptr_copy, cptr-idris"
prim__copy_pp : AnyPtr -> AnyPtr -> Bits32 -> PrimIO ()

export %foreign "C:cptr_copy, cptr-idris"
prim__copy_pb : AnyPtr -> Buffer -> Bits32 -> PrimIO ()

export %foreign "C:cptr_copy, cptr-idris"
prim__copy_bp : Buffer -> AnyPtr -> Bits32 -> PrimIO ()

export %foreign "C:cptr_inc_ptr, cptr-idris"
                "scheme,chez:(lambda (p x y) (+ p (* x y)))"
prim__inc_ptr : AnyPtr -> Bits32 -> Bits32 -> AnyPtr

--------------------------------------------------------------------------------
-- ScrubbingValue
--------------------------------------------------------------------------------

||| Default values for overwriting a C-array.
interface ScrubbingValue a where
  scrubbingvalue : a

ScrubbingValue Nat where
  scrubbingvalue = Z

ScrubbingValue Double where
  scrubbingvalue = 0

ScrubbingValue Integer where
  scrubbingvalue = 0

ScrubbingValue Int where
  scrubbingvalue = 0

ScrubbingValue Int8 where
  scrubbingvalue = 0

ScrubbingValue Int16 where
  scrubbingvalue = 0

ScrubbingValue Int32 where
  scrubbingvalue = 0

ScrubbingValue Int64 where
  scrubbingvalue = 0

ScrubbingValue Bits8 where
  scrubbingvalue = 0

ScrubbingValue Bits16 where
  scrubbingvalue = 0

ScrubbingValue Bits32 where
  scrubbingvalue = 0

ScrubbingValue Bits64 where
  scrubbingvalue = 0

--------------------------------------------------------------------------------
-- ScrubbedCArray
--------------------------------------------------------------------------------

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
|||
||| A `ScrubbedCArray` is overwritten (scrubbed) before it is freed.
export
record ScrubbedCArray (s : Type) (n : Nat) (a : Type) where
  constructor SCA
  ptr : AnyPtr

||| Convenience alias for `ScrubbedCArray' RIO`
public export
0 ScrubbedCArrayIO : Nat -> Type -> Type
ScrubbedCArrayIO = ScrubbedCArray World

public export %inline
{n : Nat} -> SizeOf a => SizeOf (ScrubbedCArray s n a) where
  sizeof_ = cast n * sizeof a

export %inline
unsafeUnwrap : ScrubbedCArray s n a -> AnyPtr
unsafeUnwrap = ptr

export %inline
unsafeWrap : AnyPtr -> ScrubbedCArray s n a
unsafeWrap = SCA

public export
0 IOBox : Type -> Type
IOBox = ScrubbedCArrayIO 1

public export
0 Box : Type -> Type -> Type
Box s = ScrubbedCArray s 1

--------------------------------------------------------------------------------
-- Scrubbing
--------------------------------------------------------------------------------

||| Overwrites a `ScrubbedCArray s n a`
||| with the `ScrubbingValue` implementation of `a`.
private
scrub :  {a : Type}
      -> {n : Nat}
      -> ScrubbingValue a
      => SetPtr a
      => SizeOf a
      -> (arr : ScrubbedCArray s n a)
      -> F1' s
scrub arr t = go arr n t
 where
  go :  (arr : ScrubbedCArray s n a)
     -> (m : Nat)
     -> (0 lt : LT m n)
     -> F1' s
  go _   Z     t =
    () # t
  go arr (S j) t =
    let _ # t := setNat arr j (scrubbingvalue a) t
     in go arr j t

--------------------------------------------------------------------------------
-- Linear API
--------------------------------------------------------------------------------

||| Allocates a new C-pointer of `sizeof a * n` bytes.
export %inline
malloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> F1 s (ScrubbedCArray s n a)
malloc1 a n t =
  let p := prim__malloc (cast n * sizeof a)
   in SCA p # t

||| Like `malloc1` but resets all allocated bytes to zero.
export %inline
calloc1 :
     (0 a : Type)
  -> {auto so : SizeOf a}
  -> (n : Nat)
  -> F1 s (ScrubbedCArray s n a)
calloc1 a n t =
  let p := prim__calloc (cast n) (sizeof a)
   in SCA p # t

||| Frees the memory allocated for a C pointer and removes it from the
||| resources bound to the linear token.
export %inline
free1 : (r : ScrubbedCArray s n a) -> F1' s
free1 r = ffi (prim__free r.ptr)

||| Extracts the first value stored in a C pointer.
export %inline
unbox : Deref a => (r : ScrubbedCArray s (S n) a) -> F1 s a
unbox r = ffi $ toPrim (deref r.ptr)

parameters {0 f      : Type -> Type}
           {0 n      : Nat}
           {0 s      : Type}
           {auto sr  : Struct f}
           {auto so  : SizeOf (f s)}
           (r        : ScrubbedCArray s n (f s))

  ||| Reads a struct from a C-pointer at the given position.
  export %inline
  getStruct : Fin n -> F1 s (f s)
  getStruct x t =
    let ptr := prim__inc_ptr r.ptr (cast $ finToNat x) (sizeof $ f s)
     in swrap ptr # t

  ||| Reads a struct from a C-pointer at the given position.
  export %inline
  getStructIx : (0 m : Nat) -> (x : Ix (S m) n) => F1 s (f s)
  getStructIx m = getStruct (ixToFin x)

  ||| Reads a struct from a C-pointer at the given position.
  export %inline
  getStructNat : (m : Nat) -> (0 lt : LT m n) => F1 s (f s)
  getStructNat m = getStruct (natToFinLT m)

parameters {0 a      : Type}
           {0 n      : Nat}
           {auto so  : SizeOf a}
           (r        : ScrubbedCArray s n a)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  get : Deref a => Fin n -> F1 s a
  get x = ffi $ toPrim (deref $ prim__inc_ptr r.ptr (cast $ finToNat x) (sizeof a))

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getIx : Deref a => (0 m : Nat) -> (x : Ix (S m) n) => F1 s a
  getIx m = get (ixToFin x)

  ||| Reads a value from a C-pointer at the given position.
  export %inline
  getNat : Deref a => (m : Nat) -> (0 lt : LT m n) => F1 s a
  getNat m = get (natToFinLT m)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  set : SetPtr a => Fin n -> a -> F1' s
  set x v = ffi $ toPrim (setPtr (prim__inc_ptr r.ptr (cast $ finToNat x) (sizeof a)) v)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setIx : SetPtr a => (0 m : Nat) -> (x : Ix (S m) n) => a -> F1' s
  setIx m = set (ixToFin x)

  ||| Writes a value to a C pointer at the given position.
  export %inline
  setNat : SetPtr a => (m : Nat) -> (0 lt : LT m n) => a -> F1' s
  setNat m = set (natToFinLT m)

  writeVect1 : SetPtr a => Vect k a -> Ix k n => F1' s
  writeVect1           []        t = () # t
  writeVect1 {k = S m} (x :: xs) t =
    let _ # t := setIx m x t
     in writeVect1 xs t

  ||| Writes the values from a vector to a C pointer
  export %inline
  writeVect : SetPtr a => Vect n a -> F1' s
  writeVect as = writeVect1 as

||| Writes the values from a list to a C pointer
export %inline
writeList :
     {auto so  : SizeOf a}
  -> {auto sp  : SetPtr a}
  -> (as       : List a)
  -> (r        : ScrubbedCArray s (length as) a)
  -> F1' s
writeList as r = writeVect r (fromList as)

||| Similar to `withCArray` for `CArray`, but ensures
||| that the allocated array is overwritten before it is freed.
export
withScrubbedCArray : ScrubbingValue a => SetPtr a => (n : Nat) -> (f : forall s . ScrubbedCArray s n a -> F1 s b) -> F1' s
withScrubbedCArray n f t =
  let r  # t := malloc1 a n t
      _  # t := f r t
      () # t := scrub r t
      () # t := free1 r t
   in () # t
