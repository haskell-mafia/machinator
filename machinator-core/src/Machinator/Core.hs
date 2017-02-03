{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core (
  -- * Versioning
    MachinatorVersion (..)
  , Versioned (..)
  -- * Errors
  , MachinatorError (..)
  , renderMachinatorError
  -- * Datatypes
  , DefinitionFile (..)
  , Definition (..)
  , DefinitionFileGraph (..)
  , buildFileGraph
  -- * Syntax
  , parseDefinitionFile
  , Pretty.ppDefinitionFile
  ) where


import           Machinator.Core.Data.Definition (DefinitionFile (..), Definition (..), DefinitionFileGraph (..))
import           Machinator.Core.Data.Version (MachinatorVersion (..), Versioned (..))
import           Machinator.Core.Graph (buildFileGraph)
import qualified Machinator.Core.Lexer as Lexer
import qualified Machinator.Core.Parser as Parser
import qualified Machinator.Core.Pretty as Pretty

import           P

import           System.IO (FilePath)


data MachinatorError
  = MParseError Parser.ParseError
  | MLexError Lexer.LexError
  deriving (Eq, Ord, Show)

renderMachinatorError :: MachinatorError -> Text
renderMachinatorError me =
  case me of
    MParseError pe ->
      "Parse error: " <> Parser.renderParseError pe
    MLexError le ->
      "Lexical error: " <> Lexer.renderLexError le


-- | Lex and parse a definition file from a 'Text' value.
--
-- The 'FilePath' is for error reporting only.
parseDefinitionFile :: FilePath -> Text -> Either MachinatorError (Versioned DefinitionFile)
parseDefinitionFile file t =
  first MLexError (Lexer.lexVersioned file t) >>= first MParseError . Parser.parseDefinitionFile file
