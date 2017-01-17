{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Haskell.Data.Types (
    HaskellTypesVersion (..)
  , HaskellTypesError (..)
  ) where


import           P


data HaskellTypesVersion
  = HaskellTypesV1
  deriving (Eq, Ord, Show, Bounded, Enum)

data HaskellTypesError
  = HaskellTypesError
  deriving (Eq, Ord, Show)
