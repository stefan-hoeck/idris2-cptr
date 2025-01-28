module Data.C.Struct

import Data.Linear.Token
import Data.C.Array
import Data.C.Deref
import Data.C.SizeOf
import Data.C.Integer

%default total

||| Interface for wrappers around `struct` pointers.
|||
||| Functions `wrap` and `unwrap` are used to convert from and
||| to the underlying pointer.
public export
interface Struct (0 f : RTag -> Type) where
  wrap       : AnyPtr -> f t
  unwrap     : f t -> AnyPtr
  structsize : Bits32

export %inline
Struct f => Deref (f t) where
  deref = pure . wrap

export %inline
(str : Struct f) => SizeOf (f t) where
  sizeof_ = structsize @{str}

export %inline
Struct f => InIO (f RIO)

||| Frees the memory allocated for a `struct`
export %inline
freeStruct : Struct f => HasIO io => f RIO -> io ()
freeStruct v = primIO $ prim__free (unwrap v)

||| Releases the memory allocated for a linear struct
export %inline
freeStruct1 : Struct f => (r : f RPure) -> (0 p : Res r rs) => C1' rs (Drop rs p)
freeStruct1 r t =
  let _ # t := ffi (prim__free (unwrap r)) t
   in unsafeRelease p t

||| Releases the memory allocated for a struct
export %inline
freeStructIO : Struct f => (r : f RIO) -> F1' [World]
freeStructIO r t = ffi (prim__free (unwrap r)) t

parameters (0 f : RTag -> Type)
           {auto str : Struct f}

  ||| Allocates memory for a single `struct` to be used in a
  ||| pure, linear computation
  export %inline
  allocStruct1 : (1 t : T1 rs) -> A1 rs (f RPure)
  allocStruct1 t =
    let m = prim__malloc (cast $ sizeof (f RPure))
     in wrap m # unsafeBind t

  ||| Allocates memory for a single `struct` resetting the
  ||| memory region to zeros.
  export %inline
  callocStruct1 : (1 t : T1 rs) -> A1 rs (f RPure)
  callocStruct1 t =
    let m = prim__calloc 1 (cast $ sizeof (f RPure))
     in wrap m # unsafeBind t

  ||| Allocates memory for a `struct` and uses it in a linear
  ||| computation.
  export
  withStruct : ((r : f RPure) -> F1 [r] a) -> a
  withStruct fun =
    run1 $ \t =>
      let str # t := allocStruct1 t
          res # t := fun str t
          _   # t := freeStruct1 str t
       in res # t

  ||| Like `withStruct` but uses `calloc` to allocate a clean memory
  ||| region.
  export
  withCleanStruct : ((r : f RPure) -> F1 [r] a) -> a
  withCleanStruct fun =
    run1 $ \t =>
      let str # t := callocStruct1 t
          res # t := fun str t
          _   # t := freeStruct1 str t
       in res # t

  ||| Allocates memory for a single `struct` to be used in a
  ||| linear computation running in `IO`
  export %inline
  allocStructIO : F1 [World] (f RIO)
  allocStructIO t =
    let m = prim__malloc (cast $ sizeof (f RPure))
     in wrap m # t

  ||| Allocates memory for a single `struct` resetting the
  ||| memory region to zeros.
  export %inline
  callocStructIO : F1 [World] (f RIO)
  callocStructIO t =
    let m = prim__calloc 1 (cast $ sizeof (f RPure))
     in wrap m # t

  ||| Allocates memory for a single `struct`
  export %inline
  allocStruct : HasIO io => io (f RIO)
  allocStruct = runIO allocStructIO

  ||| Allocates memory for a single `struct` with all bits set to 0.
  export %inline
  callocStruct : HasIO io => io (f RIO)
  callocStruct = runIO callocStructIO
