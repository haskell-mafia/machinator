{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data.Definition (
  -- * Serialisation
    DefinitionFile (..)
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

import           P

import           System.IO  (FilePath)


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
