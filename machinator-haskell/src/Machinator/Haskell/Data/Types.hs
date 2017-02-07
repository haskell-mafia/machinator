{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Haskell.Data.Types (
    HaskellTypesVersion (..)
  , HaskellTypesError (..)
  , renderHaskellTypesError
  ) where


import           P


data HaskellTypesVersion
  = HaskellTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data HaskellTypesError
  = HaskellTypesError
  deriving (Eq, Ord, Show)

renderHaskellTypesError :: HaskellTypesError -> Text
renderHaskellTypesError he =
  case he of
    HaskellTypesError ->
      "HaskellTypesError"
