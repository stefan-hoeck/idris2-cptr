module Struct

import Data.C.Ptr
import Data.C.Struct
import Hedgehog

import Syntax.T1

%default total

public export
0 Timespec : Type
Timespec = Struct "tspec" [("tv_sec", TimeT),("tv_nsec",NsecT)]

%foreign "C:cptr_allocTimespec, cptr-idris, time.h"
prim__allocTimespec : Timespec

export %inline
allocTimespec : (1 t : T1 rs) -> A1 rs Timespec
allocTimespec t =
  let p := prim__allocTimespec
   in A p (unsafeBind t)

export
withTimespec : (f : (r : Timespec) -> F1 [r] b) -> b
withTimespec f =
    run1 $ \t =>
      let A r t := allocTimespec t
          v # t := f r t
          _ # t := unsafeRelease {r} %search t
       in v # t

public export
0 Itimerspec : Type
Itimerspec = Struct "itimerspec" [("it_interval", Timespec),("it_value",Timespec)]

setGet : TimeT -> TimeT
setGet time =
  withTimespec $ \r => T1.do
    setField1 r "tv_sec" time
    getField1 r "tv_sec"

prop_timet : Property
prop_timet = property $ do
  n <- forAll $ nat (linear 0 1000)
  setGet (cast n) === cast n

export
props : Group
props =
  MkGroup "Struct"
    [ ("prop_timet", prop_timet)
    ]
