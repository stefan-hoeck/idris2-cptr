module Data.C.SizeOf

import Data.C.Integer

%default total

||| Interface for returning the size of a C object in bytes
public export
interface SizeOf a where
  sizeof_ : Nat

public export %inline
sizeof : (0 a : Type) -> SizeOf a => Nat
sizeof a = sizeof_ {a}

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

public export %inline
SizeOf Int8 where
  sizeof_ = 1

public export %inline
SizeOf Bits8 where
  sizeof_ = 1

public export %inline
SizeOf Int16 where
  sizeof_ = 2

public export %inline
SizeOf Bits16 where
  sizeof_ = 2

public export %inline
SizeOf Int32 where
  sizeof_ = 4

public export %inline
SizeOf Bits32 where
  sizeof_ = 4

public export %inline
SizeOf Int64 where
  sizeof_ = 8

public export %inline
SizeOf Bits64 where
  sizeof_ = 8
