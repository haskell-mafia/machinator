{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data (
  -- * Versioning
    MachinatorVersion (..)
  , versionToNumber
  , versionFromNumber
  , MachinatorFeature (..)
  , versionFeatures
  , featureEnabled
  , featureGuard
  , Versioned (..)
  -- * Serialisation
  , DefinitionFile (..)
  , Definition (..)
  -- * Datatype types
  , Name (..)
  , Type (..)
  , Ground (..)
  , groundToName
  , groundFromName
  , DataType (..)
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.Set  (Set)
import qualified Data.Set as S

import           P

import           System.IO  (FilePath)


-- | The version of machinator in use. This will be reflected in a
-- mandatory syntax marker and will enable or disable parser and core
-- language features.
--
-- Version should be bumped for every feature addition, feature
-- removal, or syntax change.
data MachinatorVersion
  = MachinatorV1
  deriving (Eq, Ord, Enum, Show)

versionToNumber :: Integral a => MachinatorVersion -> a
versionToNumber v =
  case v of
    MachinatorV1 ->
      1

versionFromNumber :: (Alternative f, Integral a) => a -> f MachinatorVersion
versionFromNumber i =
  case i of
    1 ->
      pure MachinatorV1
    _ ->
      empty


-- | Features supported by Machinator at one time or another.
--
-- New features should be added to this list freely, but old ones
-- should never be removed, as we (almost) never want to break
-- backwards compatibility.
data MachinatorFeature
  = HasStrings
  | HasVariants
  deriving (Eq, Ord, Enum, Show)

-- | The set of features enabled for a given version.
versionFeatures :: MachinatorVersion -> Set MachinatorFeature
versionFeatures mv =
  case mv of
    MachinatorV1 ->
      S.fromList [
          HasStrings
        , HasVariants
        ]

-- | Returns true if the given feature is enabled for the given version.
featureEnabled :: MachinatorVersion -> MachinatorFeature -> Bool
featureEnabled v f =
  S.member f (versionFeatures v)

-- | Succeeds iff the given feature is enabled for the given version.
featureGuard :: Alternative f => MachinatorVersion -> MachinatorFeature -> f ()
featureGuard v =
  guard . featureEnabled v

-- -----------------------------------------------------------------------------

-- | Functor that attaches a MachinatorVersion.
data Versioned a = Versioned MachinatorVersion a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------

-- | A set of type definitions from a given file.
data DefinitionFile
  = DefinitionFile FilePath [Definition]
  deriving (Eq, Ord, Show)

-- | A single data definition.
data Definition = Definition {
     defName :: Name
   , defType :: DataType
   } deriving (Eq, Ord, Show)

-- -----------------------------------------------------------------------------

-- | The name of a type.
newtype Name = Name {
    unName :: Text
  } deriving (Eq, Ord, Show)

-- | Types.
data Type
  = Variable Name
  | GroundT Ground
  deriving (Eq, Ord, Show)

-- | Ground types, e.g. platform primitives.
data Ground
  = StringT
  deriving (Eq, Ord, Show)

-- | Obtain the stringy form for a ground type.
groundToName :: Ground -> Name
groundToName g =
  case g of
    StringT ->
      Name "String"

-- | Obtain the ground type for a stringy name.
groundFromName :: Alternative f => Name -> f Ground
groundFromName n =
  case unName n of
    "String" ->
      pure StringT
    _ ->
      empty

-- | Declarable datatypes, e.g. sums or records.
data DataType
  = Variant (NonEmpty (Name, [Type]))
  deriving (Eq, Ord, Show)
