-- Note: This module is automatically generated when Idris builds
-- the library and the constants will be replaced with values
-- matching the system this is generated on.
--
-- The placeholders are here so that it works with tools like the LSP
-- without first compiling the library. They were generated on an x86_64
-- GNU/Linux system with GCC. If you are on a similar system, your numbers
-- might very well be identical.
module Data.C.Integer

%default total

public export
0 Short : Type
Short = Int16

public export
0 CInt : Type
CInt = Int32

public export
0 Long : Type
Long = Int64

public export
0 LongLong : Type
LongLong = Int64

public export
0 UShort : Type
UShort = Bits16

public export
0 UInt : Type
UInt = Bits32

public export
0 ULong : Type
ULong = Bits64

public export
0 ULongLong : Type
ULongLong = Bits64

public export
0 SsizeT : Type
SsizeT = Int64

public export
0 SizeT : Type
SizeT = Bits64

public export
0 ModeT : Type
ModeT = Int32

public export
0 OffT : Type
OffT = Int64

public export
0 TimeT : Type
TimeT = Int64

public export
0 SusecondsT : Type
SusecondsT = Int64

public export
0 NsecT : Type
NsecT = Int64

public export %inline
TimespecSize : Nat
TimespecSize = 16
