{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Machinator.Core.Graph (
    buildFileGraph
  ) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Machinator.Core.Data.Definition

import           P

import           System.IO (FilePath)


-- | Figures out the file graph
-- i.e. for each file, which other files does it depend on?
buildFileGraph :: [DefinitionFile] -> DefinitionFileGraph
buildFileGraph fs =
  let
    uses :: Map FilePath [Name]
    uses =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, fmap defName defs)

    binds :: Map FilePath (Set Name)
    binds =
      M.fromList . with fs $ \(DefinitionFile fp defs) ->
        (fp, fold (fmap (free . defType) defs))

    inverted :: Map Name FilePath
    inverted =
      M.foldMapWithKey (\k ns -> foldl' (\acc v -> M.insert v k acc) mempty ns) binds

    fg :: Map FilePath (Set FilePath)
    fg =
      M.fromList . with fs $ \(DefinitionFile fp _) ->
        (fp,) (maybe mempty (S.fromList . filter (/= fp) . catMaybes . fmap (flip M.lookup inverted)) (M.lookup fp uses))
  in DefinitionFileGraph fg
