{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Core.Pretty (
    ppDefinitionFile
  , ppDefinition
  ) where


import           Data.Functor.Identity (Identity(..), runIdentity)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Machinator.Core.Data.Definition
import           Machinator.Core.Data.Version

import           P

import           Text.PrettyPrint.Annotated.Leijen (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.Leijen as WL


ppDefinitionFile :: Versioned DefinitionFile -> Text
ppDefinitionFile (Versioned v df) =
  prettyUndecorated (ppDefinitionFile' v df)

-- -----------------------------------------------------------------------------

ppDefinitionFile' :: MachinatorVersion -> DefinitionFile -> Doc a
ppDefinitionFile' v (DefinitionFile _ defs) =
       ppVersion v
  WL.<$$> cat (WL.punctuate (WL.linebreak WL.<> WL.linebreak) (fmap ppDefinition defs))

ppVersion :: MachinatorVersion -> Doc a
ppVersion v =
  text "-- machinator @ v" WL.<> WL.int (versionToNumber v)

ppDefinition :: Definition -> Doc a
ppDefinition (Definition n ty) =
  case ty of
    Variant cs ->
      ppVariant n cs
    Record fts ->
      ppRecord n fts

ppVariant :: Name -> NonEmpty (Name, [Type]) -> Doc a
ppVariant (Name n) cs =
  WL.hang 2
    (text "data" <+> text n WL.<$$> text "="
      WL.<> (foldl'
              (<+>)
              WL.empty
              (WL.punctuate (WL.linebreak WL.<> text "|") (NE.toList (fmap (uncurry ppConstructor) cs)))))

ppConstructor :: Name -> [Type] -> Doc a
ppConstructor n ts =
  WL.hang 2 (ppName n WL.<> foldl' (<+>) WL.empty (fmap ppType ts))

ppRecord :: Name -> [(Name, Type)] -> Doc a
ppRecord n fts =
  WL.hang 2
    (text "record" <+> ppName n <+> text "=" <+> text "{"
      WL.<$$> foldl'
                (<+>)
                WL.empty
                (WL.punctuate (WL.linebreak WL.<> text ",") (fmap (uncurry ppRecordField) fts))
      WL.<> text "}")

ppRecordField :: Name -> Type -> Doc a
ppRecordField n ty =
  ppName n <+> text ":" <+> ppType ty

ppType :: Type -> Doc a
ppType t =
  case t of
    Variable n ->
      ppName n
    GroundT g ->
      ppGroundType g
    ListT lt ->
      "(List " <+> ppType lt <+> ")"

ppGroundType :: Ground -> Doc a
ppGroundType =
  ppName . groundToName

ppName :: Name -> Doc a
ppName =
  text . unName

-- -----------------------------------------------------------------------------

text :: Text -> Doc a
text =
  WL.string . T.unpack

pretty :: Doc a -> WL.SimpleDoc a
pretty =
  WL.renderPretty 0.4 80

cat :: [Doc a] -> Doc a
cat =
  foldl' (WL.<>) WL.empty

prettyDecorated :: (a -> Text) -> (a -> Text) -> Doc a -> Text
prettyDecorated start end =
  runIdentity . WL.displayDecoratedA str (pure . start) (pure . end) . pretty
  where
    str :: [Char] -> Identity Text
    str = pure . T.pack

prettyUndecorated :: Doc a -> Text
prettyUndecorated =
  prettyDecorated (const mempty) (const mempty)
