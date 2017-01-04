{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data (
  -- * Versioning
    MachinatorVersion (..)
  , MachinatorFeature (..)
  , versionFeatures
  , featureEnabled
  , featureGuard
  -- * Datatypes
  , Name (..)
  , Type (..)
  , DataType (..)
  ) where


import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.Set  (Set)
import qualified Data.Set as S

import           P


-- | The version of machinator in use. This will be reflected in a
-- mandatory syntax marker and will enable or disable parser and core
-- language features.
--
-- Version should be bumped for every feature addition, feature
-- removal, or syntax change.
data MachinatorVersion
  = MachinatorV1
  deriving (Eq, Ord, Enum, Show)

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

-- | Declarable datatypes, e.g. sums or records.
data DataType
  = Variant (NonEmpty (Name, [Type]))
  deriving (Eq, Ord, Show)
