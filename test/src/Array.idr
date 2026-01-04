module Array

import Data.C.Ptr
import Data.C.Array
import Data.Linear.Ref1
import Data.SOP
import Data.Vect
import Hedgehog

%default total

prop_scrub : Property
prop_scrub = do
  let vs : List Bits8
      vs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  property $ do
    ( run1 $ \t =>
       let r  # t := malloc1 Bits8 (length vs) t
           () # t := writeList vs r t
           () # t := ffi (prim__scrub (unsafeUnwrap r) (cast (length vs) * sizeof Bits8)) t
           r' # t := withIArray r toVect t
           () # t := free1 r t
        in toList r' # t ) === [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

export
props : Group
props =
  MkGroup "Array"
    [ ("prop_scrub", prop_scrub)
    ]
