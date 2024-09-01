module Struct

import Data.Linear.Traverse1
import Data.List
import Data.List1
import Data.String
import Derive.Prelude
import System
import System.File

%default total
%language ElabReflection

public export
record StructField where
  constructor SF
  ctype : String
  cname : String
  iname : String
  itype : String

%runElab derive "StructField" [Show,Eq]

public export
record Struct where
  constructor S
  ||| Name of the struct in C
  cname : String

  ||| Name of the corresponding record in Idris
  iname : String

  ||| Name of the Idris constructor
  constr : String

  ||| C-library where extractors are
  clib     : String

  ||| Fields
  fields   : List StructField

%runElab derive "Struct" [Show,Eq]

parseField : String -> Either String StructField
parseField str =
  case forget $ split (',' ==) str of
    [a,b,c,d] => Right $ SF a b c d
    _         => Left "invalid field: \{str}"

parseStruct : List String -> Either String Struct
parseStruct (c::i::con::lib::fs) =
  S c i con lib <$> traverse parseField (filter (/= "") fs)
parseStruct _ = Left "invalid struct file"

--------------------------------------------------------------------------------
-- C-Code
--------------------------------------------------------------------------------

export
callocName : Struct -> String
callocName s = "calloc_\{s.cname}"

export
calloc : Struct -> String
calloc s =
  """
  struct \{s.cname} * \{callocName s}() {
    return (struct \{s.cname} *) calloc(1, sizeof(struct \{s.cname}));
  }
  """

export
cgetterName : Struct -> StructField -> String
cgetterName s f = "get_\{s.cname}_\{f.cname}"

export
csetterName : Struct -> StructField -> String
csetterName s f = "set_\{s.cname}_\{f.cname}"

export
cgetter : Struct -> StructField -> String
cgetter s f =
  """

  \{f.ctype} \{cgetterName s f}(struct \{s.cname} * v) { return v->\{f.cname}; }
  """

export
csetter : Struct -> StructField -> String
csetter s f =
  """

  void \{csetterName s f}(struct \{s.cname} * v, \{f.ctype} val) { v->\{f.cname} = val; }
  """

export
ccode : Struct -> String
ccode s =
  unlines $
    calloc s ::
    map (cgetter s) s.fields ++
    map (csetter s) s.fields

--------------------------------------------------------------------------------
-- Idris Code
--------------------------------------------------------------------------------

export
getterFFI : Struct -> StructField -> String
getterFFI s f =
  """

  export %foreign "C:\{cgetterName s f}, \{s.clib}"
  \{cgetterName s f}: AnyPtr -> PrimIO \{f.itype}
  """

export
setterFFI : Struct -> StructField -> String
setterFFI s f =
  """

  export %foreign "C:\{csetterName s f}, \{s.clib}"
  \{csetterName s f}: AnyPtr -> \{f.itype} -> PrimIO ()
  """

export
allocFFI : Struct -> String
allocFFI s =
  """

  export %foreign "C:\{callocName s}, \{s.clib}"
  \{callocName s}: PrimIO AnyPtr
  """

var : Nat -> String
var n = "x\{show n}"

maxLength : (a -> String) -> List a -> Nat
maxLength f = foldl (\n => max n . length . f) 0

export
idrisRecord : Struct -> String
idrisRecord s =
  let fsx := zipWithIndex s.fields
   in """
      public export
      record \{s.iname} where
        constructor \{s.constr}
      \{recFields}
      %runElab derive "\{s.iname}" [Show,Eq]

      export
      to\{s.iname} : AnyPtr -> IO \{s.iname}
      to\{s.iname} p = do
      \{doStr fsx}  pure (\{conStr fsx})
      """

  where
    doField : Nat -> (Nat,StructField) -> String
    doField k (n,f) =
      "  \{padRight k ' ' $ var n} <- fromPrim $ \{cgetterName s f} p"

    doStr : List (Nat,StructField) -> String
    doStr ps =
      let ml := maxLength (var . fst) ps
       in unlines $ map (doField ml) ps

    conStr : List (Nat,StructField) -> String
    conStr ns =
      let ss := map (\p => " \{var $ fst p}") ns
       in "\{s.constr}\{concat ss}"

    recField : Nat -> StructField -> String
    recField n sf = "  \{padRight n ' ' sf.iname} : \{sf.itype}"

    recFields : String
    recFields =
      let ml := maxLength iname s.fields
       in unlines $ map (recField ml) s.fields


export
idrisFFI : Struct -> String
idrisFFI s =
  unlines $
    allocFFI s ::
    map (getterFFI s) s.fields ++
    map (setterFFI s) s.fields

export
idrisCode : Struct -> String
idrisCode s = unlines [idrisFFI s, idrisRecord s]

cbindings : List String -> IO ()
cbindings ss =
  case parseStruct ss of
    Left str => die str
    Right s  => putStrLn (ccode s)

covering
idrisBindings : List String -> IO ()
idrisBindings ss =
  case parseStruct ss of
    Left str => die str
    Right s  => putStrLn (idrisCode s)

covering
withLines : String -> (List String -> IO ()) -> IO ()
withLines pth f = do
  Right str <- readFile pth | Left err => die (show err)
  f (lines str)

covering
main : IO ()
main =
  getArgs >>= \case
    [_, "-c", file] => withLines file cbindings
    [_, "-i", file] => withLines file idrisBindings
    _               => die "Invalid args"

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

timespec : Struct
timespec =
  S
    "timespec"
    "Timespec"
    "TS"
    "cptr-idris"
    [ SF "time_t" "tv_sec" "seconds" "TimeT"
    , SF "long" "tv_nsec" "nanoseconds" "NsecT"
    ]
