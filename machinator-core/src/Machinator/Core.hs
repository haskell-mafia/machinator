{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core (
    MachinatorVersion (..)
  , Versioned (..)
  , DefinitionFile (..)
  , Definition (..)
  , parseDefinitionFile
  , Pretty.ppDefinitionFile
  ) where


import           Machinator.Core.Data
import qualified Machinator.Core.Lexer as Lexer
import qualified Machinator.Core.Parser as Parser
import qualified Machinator.Core.Pretty as Pretty

import           P

import           System.IO (FilePath)


data MachinatorError
  = MParseError Parser.ParseError
  | MLexError Lexer.LexError
  deriving (Eq, Ord, Show)


-- | Lex and parse a definition file from a 'Text' value.
--
-- The 'FilePath' is for error reporting only.
parseDefinitionFile :: FilePath -> Text -> Either MachinatorError (Versioned DefinitionFile)
parseDefinitionFile file t =
  first MLexError (Lexer.lexVersioned file t) >>= first MParseError . Parser.parseDefinitionFile file
