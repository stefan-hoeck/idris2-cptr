module Data.C.Struct

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
interface Struct a where
  wrap   : AnyPtr -> a
  unwrap : a -> AnyPtr

export %inline
Struct a => Deref a where
  deref = pure . wrap

||| Frees the memory allocated for a `struct`
export %inline
freeStruct : Struct a => HasIO io => a -> io ()
freeStruct v = primIO $ prim__free (unwrap v)

||| Allocates memory for a single `struct`
export %inline
allocStruct : (0 a : Type) -> SizeOf a => Struct a => HasIO io => io a
allocStruct a = primIO $ MkIORes (wrap $ prim__malloc (cast $ sizeof a))

||| Allocates memory for a single `struct` with all bits set to 0.
export %inline
callocStruct : (0 a : Type) -> SizeOf a => Struct a => HasIO io => io a
callocStruct a = primIO $ MkIORes (wrap $ prim__calloc 1 (cast $ sizeof a))
