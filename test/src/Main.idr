module Main

import Ptr
import Struct
import Hedgehog

%default total

main : IO ()
main =
  test
    [ Ptr.props
    , Struct.props
    ]
