{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Data.Token (
    Token (..)
  ) where


import           P


data Token
  = TData
  | TIdent Text
  | TEquals
  | TChoice
  deriving (Eq, Ord, Show)
