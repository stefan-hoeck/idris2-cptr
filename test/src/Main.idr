module Main

import Array
import Ptr
import Hedgehog

%default total

main : IO ()
main = test [ Array.props
            , Ptr.props
            ]
