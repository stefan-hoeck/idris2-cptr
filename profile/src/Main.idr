module Main

import Data.Buffer.Indexed
import Data.Array
import Data.C.Ptr
import Data.C.Array8 as A8
import Profile

%default total

sumEmptyArray : Nat -> Bits32
sumEmptyArray n = sum (Array.fill n 1)

sumEmptyBuffer : Nat -> Bits32
sumEmptyBuffer n = foldr (\x,y => cast x + y) 0 (Buffer.Indexed.fill n 1)

sumEmptyCptr : Nat -> Bits32
sumEmptyCptr n = withCArray {a = Bits32} n $ \r => withIArray r (foldr (+) 0)

sumEmptyArray8 : Nat -> Bits32
sumEmptyArray8 n = A8.withCArray n $ \r => A8.withIArray r (foldr (\x,y => cast x + y) 0)

bench : Benchmark Void
bench = Group "ref1"
  [ Group "sum IArray"
      [ Single "1"       (basic sumEmptyArray 1)
      , Single "1000"    (basic sumEmptyArray 1000)
      , Single "1000000" (basic sumEmptyArray 1000000)
      ]
  , Group "sum IBuffer"
      [ Single "1"       (basic sumEmptyBuffer 1)
      , Single "1000"    (basic sumEmptyBuffer 1000)
      , Single "1000000" (basic sumEmptyBuffer 1000000)
      ]
  , Group "sum CArray"
      [ Single "1"       (basic sumEmptyCptr 1)
      , Single "1000"    (basic sumEmptyCptr 1000)
      , Single "1000000" (basic sumEmptyCptr 1000000)
      ]
  , Group "sum CArray8"
      [ Single "1"       (basic sumEmptyArray8 1)
      , Single "1000"    (basic sumEmptyArray8 1000)
      , Single "1000000" (basic sumEmptyArray8 1000000)
      ]
  ]

main : IO ()
main = do
  runDefault (const True) Table show bench
