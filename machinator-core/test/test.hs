import           Disorder.Core.Main

import qualified Test.Machinator.Core.Parser as Parser

main :: IO ()
main =
  disorderMain [
      Parser.tests
    ]
