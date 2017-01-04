{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Machinator.Core.Arbitrary where


import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Disorder.Corpus
import           Disorder.Jack

import           Machinator.Core.Data.Definition
import           Machinator.Core.Data.Version

import           P


-- TODO this should probably branch on version at relevant points
genDefinitionFileV1 :: Jack (Versioned DefinitionFile)
genDefinitionFileV1 =
  sized $ \n ->
    fmap
      (Versioned MachinatorV1 . DefinitionFile "Test.Machinator.Core.Arbitrary")
      (listOfN 0 n (genDefinitionV1 (n `div` 2)))

genDefinitionV1 :: Int -> Jack Definition
genDefinitionV1 n =
  Definition
    <$> genName
    <*> genVariantV1 n

genVariantV1 :: Int -> Jack DataType
genVariantV1 k =
  (Variant . NE.fromList)
    <$> (listOfN 1 k $ do
          n <- genName
          ts <- listOfN 0 10 genTypeV1
          pure (n, ts))

genTypeV1 :: Jack Type
genTypeV1 =
  oneOf [
      Variable <$> genName
    , GroundT <$> genGroundTypeV1
    ]

genGroundTypeV1 :: Jack Ground
genGroundTypeV1 =
  elements [
      StringT
    ]

genName :: Jack Name
genName =
  fmap (Name . T.toTitle) $ oneOf [
      elements waters
    , T.pack <$> vectorOf 8 (arbitrary `suchThat` Char.isAsciiLower)
    ]
