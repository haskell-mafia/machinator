{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Lexer (
    lexVersioned
  , LexError (..)
  , renderLexError
  ) where


import qualified Data.Text as T

import           Machinator.Core.Data.Position
import           Machinator.Core.Data.Token
import           Machinator.Core.Data.Version

import           P

import           System.IO  (FilePath)

import qualified Text.Megaparsec.Lexer as ML
import qualified Text.Megaparsec as M
import           Text.Megaparsec.Text  (Parser)


data LexError
  = LexError Text
  deriving (Eq, Ord, Show)

renderLexError :: LexError -> Text
renderLexError e =
  case e of
    LexError t ->
      t

lexVersioned :: FilePath -> Text -> Either LexError (Versioned [Positioned Token])
lexVersioned file t =
  first (LexError . T.pack . M.parseErrorPretty) (M.runParser lexVersioned' file t)


-- -----------------------------------------------------------------------------


lexVersioned' :: Parser (Versioned [Positioned Token])
lexVersioned' = do
  mv <- version
  ts <- many token
  pure (Versioned mv ts)

version :: Parser MachinatorVersion
version = do
  string "-- machinator @ v"
  v <- ML.integer
  _ <- M.newline
  space
  case versionFromNumber v of
    Just ver ->
      pure ver
    Nothing ->
      -- TODO custom error component for bad version number?
      fail ("Unknown version number " <> show v <> " - expected 1")

token :: Parser (Positioned Token)
token =
  token' <* space

token' :: Parser (Positioned Token)
token' =
  withPosition $ M.choice [
      string "data" *> pure TData
    , string "record" *> pure TRecord
    , string "=" *> pure TEquals
    , string "|" *> pure TChoice
    , string "(" *> pure TLParen
    , string ")" *> pure TRParen
    , string "{" *> pure TLBrace
    , string "}" *> pure TRBrace
    , string ":" *> pure TTypeSig
    , string "," *> pure TComma
    , ident
    ]

ident :: Parser Token
ident = do
  s <- some M.alphaNumChar
  pure (TIdent (T.pack s))

-- -----------------------------------------------------------------------------

space :: Parser ()
space =
  many M.spaceChar *> pure ()
{-# INLINEABLE space #-}

string :: [Char] -> Parser ()
string s =
  M.string s *> pure ()
{-# INLINEABLE string #-}

getPosition :: Parser Position
getPosition =
  fmap posPosition M.getPosition
{-# INLINEABLE getPosition #-}

withPosition :: Parser a -> Parser (Positioned a)
withPosition p = do
  a <- getPosition
  b <- p
  c <- getPosition
  pure (b :@ Range a c)
{-# INLINEABLE withPosition #-}

posPosition :: M.SourcePos -> Position
posPosition (M.SourcePos file line col) =
  Position (fromIntegral (M.unPos line)) (fromIntegral (M.unPos col)) file
{-# INLINEABLE posPosition #-}
