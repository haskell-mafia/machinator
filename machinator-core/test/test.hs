import           Disorder.Core.Main

import qualified Test.Machinator.Core.Parser as Parser
import qualified Test.Machinator.Core.Graph as Graph
import qualified Test.Machinator.Core.Lexer as Lexer

main :: IO ()
main =
  disorderMain [
      Parser.tests
    , Graph.tests
    , Lexer.tests
    ]
