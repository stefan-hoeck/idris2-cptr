module Data.C.Struct

import public Data.List.Quantifiers
import public System.FFI
import Data.Linear.Ref1
import Data.C.Array
import Data.C.Deref
import Data.C.SizeOf
import Data.C.Integer

%default total

namespace IO

  ||| Releases the memory allocated for a `Struct`.
  export %inline
  freeStruct : Struct s fs -> IO ()
  freeStruct s = fromPrim (prim__free $ believe_me s)

  parameters {s : String}
             (r  : Struct s fs)
             (nm : String)
             {auto prf : FieldType nm t fs}

    ||| Retrieve the value of the specified field in the given `Struct`.
    export %inline
    getFieldIO : IO t
    getFieldIO = pure $ getField {sn = s} r nm @{prf}

    ||| Set the value of the specified field in the given `Struct`.
    export %inline
    setFieldIO : (val : t) -> IO ()
    setFieldIO val = setField {sn = s} r nm @{prf} val

namespace Linear
  parameters {s : String}
             (r  : Struct s fs)
             (nm : String)
             {auto prf : FieldType nm t fs}

    ||| Retrieve the value of the specified field in the given `Struct`.
    export %inline
    getField1 : (0 p : Res r rs) => F1 rs t
    getField1 t = getField {sn = s} r nm @{prf} # t

    export %inline
    setField1 : (0 p : Res r rs) => (val : t) -> F1' rs
    setField1 val = ffi $ toPrim $ setField {sn = s} r nm @{prf} val
