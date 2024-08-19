module Data.C.Deref

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "C:cptr_deref_bits8, cptr-idris"
prim__deref_bits8 : AnyPtr -> PrimIO Bits8

%foreign "C:cptr_deref_bits16, cptr-idris"
prim__deref_bits16 : AnyPtr -> PrimIO Bits16

%foreign "C:cptr_deref_bits32, cptr-idris"
prim__deref_bits32 : AnyPtr -> PrimIO Bits32

%foreign "C:cptr_deref_bits64, cptr-idris"
prim__deref_bits64 : AnyPtr -> PrimIO Bits64

%foreign "C:cptr_deref_int8, cptr-idris"
prim__deref_int8 : AnyPtr -> PrimIO Int8

%foreign "C:cptr_deref_int16, cptr-idris"
prim__deref_int16 : AnyPtr -> PrimIO Int16

%foreign "C:cptr_deref_int32, cptr-idris"
prim__deref_int32 : AnyPtr -> PrimIO Int32

%foreign "C:cptr_deref_int64, cptr-idris"
prim__deref_int64 : AnyPtr -> PrimIO Int64

%foreign "C:cptr_set_bits8, cptr-idris"
prim__set_bits8 : AnyPtr -> Bits8 -> PrimIO ()

%foreign "C:cptr_set_bits16, cptr-idris"
prim__set_bits16 : AnyPtr -> Bits16 -> PrimIO ()

%foreign "C:cptr_set_bits32, cptr-idris"
prim__set_bits32 : AnyPtr -> Bits32 -> PrimIO ()

%foreign "C:cptr_set_bits64, cptr-idris"
prim__set_bits64 : AnyPtr -> Bits64 -> PrimIO ()

%foreign "C:cptr_set_int8, cptr-idris"
prim__set_int8 : AnyPtr -> Int8 -> PrimIO ()

%foreign "C:cptr_set_int16, cptr-idris"
prim__set_int16 : AnyPtr -> Int16 -> PrimIO ()

%foreign "C:cptr_set_int32, cptr-idris"
prim__set_int32 : AnyPtr -> Int32 -> PrimIO ()

%foreign "C:cptr_set_int64, cptr-idris"
prim__set_int64 : AnyPtr -> Int64 -> PrimIO ()

--------------------------------------------------------------------------------
-- Interfaces
--------------------------------------------------------------------------------

public export
interface Deref a where
  deref : AnyPtr -> IO a

export %inline
Deref Bits8 where deref p = fromPrim $ prim__deref_bits8 p

export %inline
Deref Bits16 where deref p = fromPrim $ prim__deref_bits16 p

export %inline
Deref Bits32 where deref p = fromPrim $ prim__deref_bits32 p

export %inline
Deref Bits64 where deref p = fromPrim $ prim__deref_bits64 p

export %inline
Deref Int8 where deref p = fromPrim $ prim__deref_int8 p

export %inline
Deref Int16 where deref p = fromPrim $ prim__deref_int16 p

export %inline
Deref Int32 where deref p = fromPrim $ prim__deref_int32 p

export %inline
Deref Int64 where deref p = fromPrim $ prim__deref_int64 p


public export
interface SetPtr a where
  setPtr : AnyPtr -> a -> IO ()

export %inline
SetPtr Bits8 where setPtr p x = fromPrim $ prim__set_bits8 p x

export %inline
SetPtr Bits16 where setPtr p x = fromPrim $ prim__set_bits16 p x

export %inline
SetPtr Bits32 where setPtr p x = fromPrim $ prim__set_bits32 p x

export %inline
SetPtr Bits64 where setPtr p x = fromPrim $ prim__set_bits64 p x

export %inline
SetPtr Int8 where setPtr p x = fromPrim $ prim__set_int8 p x

export %inline
SetPtr Int16 where setPtr p x = fromPrim $ prim__set_int16 p x

export %inline
SetPtr Int32 where setPtr p x = fromPrim $ prim__set_int32 p x

export %inline
SetPtr Int64 where setPtr p x = fromPrim $ prim__set_int64 p x
