module Main

import Ptr
import Hedgehog

%default total

main : IO ()
main = test [ Ptr.props ]
