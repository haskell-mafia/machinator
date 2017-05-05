{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Purescript.Data.Types (
    PurescriptTypesVersion (..)
  , PurescriptTypesError (..)
  , renderPurescriptTypesError
  ) where


import           P


data PurescriptTypesVersion
  = PurescriptTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data PurescriptTypesError
  = PurescriptTypesError
  deriving (Eq, Ord, Show)

renderPurescriptTypesError :: PurescriptTypesError -> Text
renderPurescriptTypesError pe =
  case pe of
    PurescriptTypesError ->
      "PurescriptTypesError"
