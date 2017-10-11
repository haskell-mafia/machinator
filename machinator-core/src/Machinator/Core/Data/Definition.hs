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
  , DefinitionFileGraph (..)
  -- * Datatype types
  , Name (..)
  , Type (..)
  , Ground (..)
  , groundToName
  , groundFromName
  , DataType (..)
  -- * Traversals etc
  , free
  ) where


import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as S

import           P

import           System.IO  (FilePath)


-- | A set of type definitions from a given file.
data DefinitionFile a
  = DefinitionFile FilePath [Definition a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | A single data definition.
data Definition a = Definition {
     defName :: Name
   , defType :: DataType a
   , defAnnotation :: a
   } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | The module graph.
-- Maps each file to the other files it depends on.
newtype DefinitionFileGraph = DefinitionFileGraph {
    unDefinitionFileGraph :: Map FilePath (Set FilePath)
  } deriving (Eq, Ord, Show, Monoid)


-- -----------------------------------------------------------------------------

-- | The name of a type.
newtype Name = Name {
    unName :: Text
  } deriving (Eq, Ord, Show)

-- | Types.
data Type
  = Variable Name
  | GroundT Ground
  | ListT Type
  deriving (Eq, Ord, Show)

-- | Ground types, e.g. platform primitives.
data Ground
  = StringT
  | BoolT
  deriving (Eq, Ord, Show)

-- | Obtain the stringy form for a ground type.
groundToName :: Ground -> Name
groundToName g =
  case g of
    StringT ->
      Name "String"
    BoolT ->
      Name "Bool"

-- | Obtain the ground type for a stringy name.
groundFromName :: Alternative f => Name -> f Ground
groundFromName n =
  case unName n of
    "String" ->
      pure StringT
    "Bool" ->
      pure BoolT
    _ ->
      empty

-- | Declarable datatypes, e.g. sums or records.
data DataType a
  = Variant (NonEmpty (Name, [Type])) a
  | Record [(Name, Type)] a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- -----------------------------------------------------------------------------

free :: DataType a -> Set Name
free d =
  case d of
    Variant nts _a ->
      fold . with nts $ \(_, ts) ->
        S.fromList . catMaybes . with ts $ freeInType
    Record fts _a ->
      S.fromList . catMaybes . with fts $ \(_, t) -> (freeInType t)

freeInType :: Type -> Maybe Name
freeInType t =
  case t of
    Variable n ->
      pure n
    GroundT _ ->
      empty
    ListT lt ->
      freeInType lt
