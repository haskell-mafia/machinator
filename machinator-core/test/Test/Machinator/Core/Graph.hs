{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Machinator.Core.Graph where


import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Disorder.Core
import           Disorder.Jack hiding (variant)

import           Machinator.Core.Data.Definition
import           Machinator.Core.Graph

import           P

import           System.IO (IO)


-- FIX these are some shameful unit tests
--     there's equivalent properties and generators in projector-html

prop_filegraph_unit :: Property
prop_filegraph_unit =
  once (buildFileGraph testDefs === testGraph)

testDefs :: [DefinitionFile ()]
testDefs = [
    DefinitionFile "foo.mcn" [
        (variant (Name "Foo") $
             (Name "Foo", [GroundT StringT, GroundT StringT, Variable (Name "Baz")]) :| [])
      , (variant (Name "Bar") $
             (Name "BarFoo", [Variable (Name "Foo")])
          :| [ (Name "BarBaz", [Variable (Name "Baz")])
             , (Name "BarFooBaz", [Variable (Name "Foo"), Variable (Name "Baz")])
             ])
      , (variant (Name "Baz") $
             (Name "Baz", []) :| [])
      ]
  , DefinitionFile "heck.mcn" [
        (variant (Name "Heck") $
          (Name "HeckFooBar", [Variable (Name "Foo"), Variable (Name "Bar")]) :| [])
      , (variant (Name "Hell") $
          (Name "HellBaz", [Variable (Name "Baz")]) :| [])
      ]
  , DefinitionFile "pel.mcn" [
        (variant (Name "Pel") $
             (Name "Pel", [Variable (Name "Heck"), Variable (Name "Hell")])
          :| [(Name "Pil", [Variable (Name "Foo"), Variable (Name "Bar")])])
      ]
  ]

variant :: Name -> NonEmpty (Name, [Type]) -> Definition ()
variant n v =
  Definition n (Variant v ()) ()

testGraph :: DefinitionFileGraph
testGraph =
  DefinitionFileGraph $ M.fromList [
      ("foo.mcn", S.fromList [])
    , ("heck.mcn", S.fromList ["foo.mcn"])
    , ("pel.mcn", S.fromList ["foo.mcn", "heck.mcn"])
    ]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
