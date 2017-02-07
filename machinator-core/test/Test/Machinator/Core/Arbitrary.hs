{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Machinator.Core.Arbitrary where


import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S
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
      (Versioned MachinatorV1 . DefinitionFile "Test.Machinator.Core.Arbitrary") $ do
        tns <- genFreeNames n mempty
        fmap fst
          (foldM
            (\(res, kc) name -> do
              (d, kc') <- genDefinitionV1' (n `div` 2) name tns kc
              pure (d:res, kc'))
            (mempty, mempty)
            (toList tns))

genDefinitionV1' :: Int -> Name -> Set Name -> Set Name -> Jack (Definition, Set Name)
genDefinitionV1' k n knownTypes knownCons = do
  (v, cons) <- genVariantV1 k (S.insert n knownTypes) knownCons
  pure (Definition n v, knownCons <> cons)

genVariantV1 :: Int -> Set Name -> Set Name -> Jack (DataType, Set Name)
genVariantV1 k knownTypes knownCons = do
  k' <- chooseInt (1, k+1)
  ns <- genFreeNames k' knownCons
  cts <- for (toList ns) $ \n -> fmap (n,) (listOfN 0 10 (genTypeV1 knownTypes))
  pure (Variant (NE.fromList cts), ns <> knownCons)

genTypeV1 :: Set Name -> Jack Type
genTypeV1 knownTypes =
  let genVar = fmap Variable (elements (toList knownTypes))
      genGround = fmap GroundT genGroundTypeV1
      genList = fmap ListT (genTypeV1 knownTypes)
  in if S.null knownTypes then genGround else oneOf [genVar, genGround, genList]

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

genFreeNames :: Int -> Set Name -> Jack (Set Name)
genFreeNames k known =
  go k genName known mempty
  where
    go 0 _ _ r = pure r
    go j g s r = do
      e <- g `suchThat` (`S.notMember` s)
      go (j-1) g (S.insert e s) (S.insert e r)
