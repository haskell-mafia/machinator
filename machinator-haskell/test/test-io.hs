import Disorder.Core.Main

import Test.IO.Machinator.Haskell.Scheme.Types as Types

main :: IO ()
main = disorderMain [
    Types.tests
  ]
