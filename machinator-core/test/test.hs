import           Disorder.Core.Main

import qualified Test.Machinator.Core.Parser as Parser
import qualified Test.Machinator.Core.Graph as Graph

main :: IO ()
main =
  disorderMain [
      Parser.tests
    , Graph.tests
    ]
