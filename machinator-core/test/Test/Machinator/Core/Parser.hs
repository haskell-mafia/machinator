{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Machinator.Core.Parser (tests) where

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           Machinator.Core

import           P

import           Test.Machinator.Core.Arbitrary


prop_tripping_v1 =
  gamble genDefinitionFileV1 $ \vdf ->
    tripping ppDefinitionFile (parseDefinitionFile "Test.Machinator.Core.Arbitrary") vdf

prop_tripping_v2 =
  gamble genDefinitionFileV2 $ \vdf ->
    tripping ppDefinitionFile (parseDefinitionFile "Test.Machinator.Core.Arbitrary") vdf


return []
tests = $disorderCheckEnvAll TestRunNormal
