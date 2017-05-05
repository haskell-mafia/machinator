{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Purescript.Scheme.Types.Codegen (
    genTypesV1
  , genUnwrapV1
  ) where


import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Machinator.Core
import           Machinator.Core.Data.Definition

import           P

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>), (<##>))
import qualified Text.PrettyPrint.Annotated.WL as WL


-- | Generates a type declaration for the given definition.
genTypesV1 :: Definition -> Text
genTypesV1 (Definition (Name n) dec) =
  renderText $ case dec of
    Variant (c1 :| cts) ->
      WL.hang 2 $
        string "data" <+> text n <##> string "="
          <+> fold (WL.punctuate (WL.hardline <> string "| ") (fmap (uncurry genConstructorV1) (c1:cts)))
    Record fts ->
      WL.hang 2 $
        string "newtype" <+> text n <+> string "=" <+> text n <+> genRecordV1 fts

genTypeV1 :: Type -> Doc a
genTypeV1 ty =
  case ty of
    Variable (Name n) ->
      text n
    GroundT g ->
      case g of
        StringT ->
          text "String"
    ListT t2 ->
      WL.parens (string "Array" <+> genTypeV1 t2)

genConstructorV1 :: Name -> [Type] -> Doc a
genConstructorV1 (Name n) tys =
  text n <+> WL.hsep (fmap genTypeV1 tys)

-- | Generates a naked record for the given definition.
--
-- @
-- [(Name "foo", GroundT StringT), (Name "bar", GroundT StringT)]
-- { foo :: String, bar :: String }
-- @
genRecordV1 :: [(Name, Type)] -> Doc a
genRecordV1 fts =
  WL.hang 2 $
    WL.encloseSep (string "{") (string "}") (string ",") $
      with fts $ \(Name n, ty) ->
        text n <+> string "::" <+> genTypeV1 ty

-- | Creates an unwrap function for records. This is a little stubborn as we target 0.9.3.
--
-- @
-- newtype Hello = Hello { foo :: Boolean }
-- unHello :: Hello -> { foo :: Boolean }
-- unHello (Hello h) = h
-- @
genUnwrapV1 :: Definition -> Maybe Text
genUnwrapV1 (Definition (Name n) dec) =
  renderText <$> case dec of
    Variant _ ->
      empty
    Record fs ->
      Just $
             text ("un" <> n) <+> string "::" <+> text n <+> "->" <+> genRecordV1 fs
        <##> text ("un" <> n) <+> WL.parens (text n <+> string "x") <+> string "=" <+> string "x"

-- -----------------------------------------------------------------------------

string :: [Char] -> Doc a
string =
  WL.text

text :: Text -> Doc a
text =
  WL.text . T.unpack

renderText :: Doc a -> Text
renderText =
  TL.toStrict . WL.displayT . WL.renderPrettyDefault
