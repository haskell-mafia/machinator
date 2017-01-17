{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Machinator.Haskell.Scheme.Types (
    types
  , typesV1
  ) where


import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           Machinator.Core
import qualified Machinator.Core.Graph as MG
import           Machinator.Haskell.Data.Types
import           Machinator.Haskell.TH.Types

import qualified Language.Haskell.TH as TH

import           P

import qualified System.FilePath.Posix as FilePath
import           System.IO (FilePath)


types :: HaskellTypesVersion -> [DefinitionFile] -> Either HaskellTypesError [(FilePath, Text)]
types v ds =
  case v of
    HaskellTypesV1 ->
      typesV1 ds

typesV1 :: [DefinitionFile] -> Either HaskellTypesError [(FilePath, Text)]
typesV1 dfs =
  let DefinitionFileGraph fg = MG.buildFileGraph dfs
      mg = M.mapKeys filePathToModuleName (fmap (S.map filePathToModuleName) fg)
  in for dfs $ \(DefinitionFile fp defs) ->
       let mn = filePathToModuleName fp in
       pure (genFileName mn, renderModule mn mg defs)

-- -----------------------------------------------------------------------------

-- Extremely shoddy codegen for v0 purposes
renderModule :: ModuleName -> Map ModuleName (Set ModuleName) -> [Definition] -> Text
renderModule mn@(ModuleName n) imports defs =
  fold [
      "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , T.unwords ["module", n, "where"]
    , "import Data.Text (Text)"
    , maybe mempty (T.unlines . fmap renderImport . toList) (M.lookup mn imports)
    , T.unlines (fmap (T.pack . TH.pprint . genTypesV1) defs)
    ]

renderImport :: ModuleName -> Text
renderImport (ModuleName n) =
  "import " <> n

-- -----------------------------------------------------------------------------

newtype ModuleName = ModuleName {
    _unModuleName :: Text
  } deriving (Eq, Ord, Show)

-- TODO I don't know if this logic belongs here.

-- | Derive a module name from the relative 'FilePath'.
--
-- @
-- λ> filePathToModuleName "./path_to/my/favourite_Template_place.hs"
-- ModuleName {unModuleName = "PathTo.My.FavouriteTemplatePlace"}
-- @
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
  T.unpack (T.replace "." "/" n) <> ".hs"
