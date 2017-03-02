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
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TComma
  | TTypeSig
  | TRecord
  deriving (Eq, Ord, Show)
