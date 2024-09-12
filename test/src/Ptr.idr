module Ptr

import Data.C.Ptr
import Data.Linear.Ref1
import Data.SOP
import Data.Vect
import Hedgehog

import Syntax.T1

%default total

%hide Data.SOP.NP.get

parameters {0 a : Type}
           {auto so : SizeOf a}
           {auto df : Deref a}
           {auto sp : SetPtr a}

  setGet : (n : Nat) -> Fin n -> a -> a
  setGet n x v =
    withCArray n $ \r => T1.do
      set r x v
      get r x

  fromToList : List a -> List a
  fromToList as =
    withCArray (length as) $ \r => T1.do
      writeVect r (fromList as)
      v <- readVect r
      pure (toList v)

  roundTrip : Show a => Eq a => Gen a -> Property
  roundTrip as = property $ do
    [n,v] <- forAll $ np [nat (linear 0 100), as]
    x     <- forAll $ fin (linearFin n)
    setGet (S n) x v === v

prop_bits8 : Property
prop_bits8 = roundTrip anyBits8

prop_bits16 : Property
prop_bits16 = roundTrip anyBits16

prop_bits32 : Property
prop_bits32 = roundTrip anyBits32

prop_bits64 : Property
prop_bits64 = roundTrip anyBits64

prop_int8 : Property
prop_int8 = roundTrip anyInt8

prop_int16 : Property
prop_int16 = roundTrip anyInt16

prop_int32 : Property
prop_int32 = roundTrip anyInt32

prop_int64 : Property
prop_int64 = roundTrip anyInt64

anyStr : Gen String
anyStr = string (linear 0 30) printableUnicode

prop_str : Property
prop_str = roundTrip anyStr

prop_strMaybe : Property
prop_strMaybe = roundTrip (maybe anyStr)

prop_strList : Property
prop_strList =
  property $ do
    vs <- forAll (list (linear 0 20) anyStr)
    fromToList vs === vs

prop_strMaybeList : Property
prop_strMaybeList =
  property $ do
    vs <- forAll (list (linear 0 20) (maybe anyStr))
    fromToList vs === vs

export
props : Group
props =
  MkGroup "Ptr"
    [ ("prop_bits8", prop_bits8)
    , ("prop_bits16", prop_bits16)
    , ("prop_bits32", prop_bits32)
    , ("prop_bits64", prop_bits64)
    , ("prop_int8", prop_int8)
    , ("prop_int16", prop_int16)
    , ("prop_int32", prop_int32)
    , ("prop_int64", prop_int64)
    , ("prop_str", prop_str)
    , ("prop_strMaybe", prop_strMaybe)
    , ("prop_strList", prop_strList)
    , ("prop_strMaybeList", prop_strMaybeList)
    ]
