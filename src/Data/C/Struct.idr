module Data.C.Struct

import Control.Monad.Resource
import Data.C.Integer
import Data.C.SizeOf
import Data.Linear.ELift1
import Data.Linear.Token

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

--------------------------------------------------------------------------------
-- WrappedPtr
--------------------------------------------------------------------------------

public export
interface WrappedPtr (0 a : Type) where
  wrap   : AnyPtr -> a
  unwrap : a -> AnyPtr

--------------------------------------------------------------------------------
-- Struct
--------------------------------------------------------------------------------

||| Interface for wrappers around `struct` pointers.
|||
||| Functions `wrap` and `unwrap` are used to convert from and
||| to the underlying pointer.
public export
interface Struct (0 f : Type -> Type) where
  swrap   : AnyPtr -> f s
  sunwrap : f s -> AnyPtr

export %inline
Struct f => WrappedPtr (f s) where
  wrap   = swrap
  unwrap = sunwrap

||| Frees the memory allocated for a `struct`
export %inline
freeStruct1 : Struct f => f s -> F1' s
freeStruct1 v = ffi $ prim__free (sunwrap v)

||| Allocates memory for a single `struct`
export %inline
allocStruct1 : (0 f : Type -> Type) -> SizeOf (f s) => Struct f => F1 s (f s)
allocStruct1 f t = swrap (prim__malloc (cast $ sizeof (f s))) # t

||| Allocates memory for a single `struct`
export %inline
callocStruct1 : (0 f : Type -> Type) -> SizeOf (f s) => Struct f => F1 s (f s)
callocStruct1 f t = swrap (prim__calloc 1 (cast $ sizeof (f s))) # t

parameters {auto l1 : Lift1 s m}
  ||| Frees the memory allocated for a `struct`
  export %inline
  freeStruct : Struct f => f s -> m ()
  freeStruct v = lift1 (freeStruct1 v)

  ||| Allocates memory for a single `struct` with all bits set to 0.
  export %inline
  allocStruct : (0 f : _) -> SizeOf (f s) => Struct f => m (f s)
  allocStruct f = lift1 (allocStruct1 f)

  ||| Allocates memory for a single `struct` with all bits set to 0.
  export %inline
  callocStruct : (0 f : _) -> SizeOf (f s) => Struct f => m (f s)
  callocStruct f = lift1 (callocStruct1 f)

--------------------------------------------------------------------------------
-- Resource
--------------------------------------------------------------------------------

export %inline
ELift1 s m => Struct f => Resource m (f s) where
  cleanup v = lift1 (freeStruct1 v)
