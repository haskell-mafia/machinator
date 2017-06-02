{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Machinator.Core.Lexer where

import qualified Data.Text as T

import           Disorder.Core hiding (tripping)
import           Disorder.Jack

import           Machinator.Core.Data.Position
import           Machinator.Core.Data.Token
import           Machinator.Core.Data.Version
import           Machinator.Core.Lexer

import           P


prop_lexer_v1_no_comments =
  let
    r =
      lexVersioned "lexer_test" $
        T.unlines [
            "-- machinator @ v1"
          , "data Bar = Bar " <> "-" <> "- data Foo"
          ]
  in
    once $
      isLeft r

prop_lexer_v2_comments =
  let
    r =
      lexVersioned "lexer_test" $
        T.unlines [
            "-- machinator @ v2"
          , "{" <> "-"
          , "  data Foo"
          , "     -} data Foo = Foo"
          , "data Bar = Bar " <> "-" <> "- data Foo"
          ]
  in
    once $
      fmap (fmap (fmap extractPositioned)) r
      ===
      Right (Versioned MachinatorV2 [
          TData , TIdent "Foo" , TEquals , TIdent "Foo"
        , TData , TIdent "Bar" , TEquals , TIdent "Bar"
        ])


return []
tests = $disorderCheckEnvAll TestRunNormal
