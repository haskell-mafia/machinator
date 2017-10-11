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
    tripping ppDefinitionFile parseDefinitionFile' vdf

prop_tripping_v2 =
  gamble genDefinitionFileV2 $ \vdf ->
    tripping ppDefinitionFile parseDefinitionFile' vdf


parseDefinitionFile' :: Text -> Either MachinatorError (Versioned (DefinitionFile ()))
parseDefinitionFile' =
  dropLocations . parseDefinitionFile "Test.Machinator.Core.Arbitrary"

dropLocations ::
     Either MachinatorError (Versioned (DefinitionFile a))
  -> Either MachinatorError (Versioned (DefinitionFile ()))
dropLocations =
  fmap (fmap (fmap (const ())))

return []
tests = $disorderCheckEnvAll TestRunNormal
