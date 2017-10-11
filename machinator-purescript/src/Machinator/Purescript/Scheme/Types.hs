{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Machinator.Purescript.Scheme.Types where


import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Machinator.Core
import qualified Machinator.Core.Graph as MG
import           Machinator.Purescript.Data.Types
import qualified Machinator.Purescript.Scheme.Types.Codegen as Codegen

import           P

import qualified System.FilePath.Posix as FilePath
import           System.IO (FilePath)


types :: PurescriptTypesVersion -> [DefinitionFile a] -> Either PurescriptTypesError [(FilePath, Text)]
types v ds =
  case v of
    PurescriptTypesV1 ->
      typesV1 ds

typesV1 :: [DefinitionFile a] -> Either PurescriptTypesError [(FilePath, Text)]
typesV1 dfs =
  let DefinitionFileGraph fg = MG.buildFileGraph dfs
      mg = M.mapKeys filePathToModuleName (fmap (S.map filePathToModuleName) fg)
  in for dfs $ \(DefinitionFile fp defs) ->
       let mn = filePathToModuleName fp in
       pure (genFileName mn, renderModule mn mg defs)

-- -----------------------------------------------------------------------------

renderModule :: ModuleName -> Map ModuleName (Set ModuleName) -> [Definition a] -> Text
renderModule mn@(ModuleName n) imports defs =
  T.unlines [
      T.unwords ["module", n, "where"]
    , maybe mempty (T.unlines . fmap renderImport . toList) (M.lookup mn imports)
    , T.unlines . flip foldMap defs $ \def ->
        Codegen.genTypesV1 def : maybeToList (Codegen.genUnwrapV1 def)
    ]

renderImport :: ModuleName -> Text
renderImport (ModuleName n) =
  "import " <> n

-- -----------------------------------------------------------------------------

newtype ModuleName = ModuleName {
    _unModuleName :: Text
  } deriving (Eq, Ord, Show)


-- | Derive a module name from the relative 'FilePath'.
--
-- @
-- λ> filePathToModuleName "./path_to/my/favourite_Template_place.hs"
-- ModuleName {unModuleName = "PathTo.My.FavouriteTemplatePlace"}
-- @
--
-- TODO V2 Accept an alternative function as a parameter.
filePathToModuleName :: FilePath -> ModuleName
filePathToModuleName =
  ModuleName . T.pack . goUpper . FilePath.dropExtension
  where
    goUpper [] = []
    goUpper (x:xs)
      | Char.isAlphaNum x = Char.toUpper x : go xs
      | otherwise = goUpper xs
    go [] = []
    go (x:xs)
      | x == '/' = '.' : goUpper xs
      | Char.isAlphaNum x = x : go xs
      | otherwise = goUpper xs

genFileName :: ModuleName -> FilePath
genFileName (ModuleName n) =
  T.unpack (T.replace "." "/" n) <> ".purs"
