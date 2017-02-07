{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Machinator.Haskell.Scheme.Types where


import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Core.IO
import           Disorder.Jack

import           Machinator.Core
import           Machinator.Haskell.Data.Types
import qualified Machinator.Haskell.Scheme.Types as MH

import           P

import System.Directory
import           System.Exit (ExitCode(..))
import           System.FilePath.Posix ((</>), takeDirectory)
import           System.IO (FilePath, IO)
import           System.IO.Temp (withTempDirectory)
import           System.Process (readProcessWithExitCode)

import           Test.Machinator.Core.Arbitrary


-- -----------------------------------------------------------------------------

prop_typesV1 :: Property
prop_typesV1 =
  gamble genDefinitionFilesV1 $ \files ->
    either (\e -> counterexample (T.unpack (renderHaskellTypesError e)) (property False)) id $ do
      fts <- MH.typesV1 (fmap (\(Versioned _ df) -> df) files)
      pure . tmpDirProp $ \stem -> do
        let filemap = fmap (first (stem </>)) fts
        for_ filemap $ \(fp, txt) -> do
          T.writeFile fp txt
          createDirectoryIfMissing True (takeDirectory ("/tmp" </> fp))
          T.writeFile ("/tmp" </> fp) txt
        ghcProp (fmap fst filemap)

-- -----------------------------------------------------------------------------

-- Compiles with GHC in the current sandbox, failing if exit status is nonzero.
ghcProp :: [FilePath] -> IO Property
ghcProp path =
  fmap
    (processProp (const (property True)))
    (readProcessWithExitCode "cabal" (["exec", "--", "ghc"] <> path) "")

-- -----------------------------------------------------------------------------

processProp :: ([Char] -> Property) -> (ExitCode, [Char], [Char]) -> Property
processProp f (code, out, err) =
  case code of
    ExitSuccess ->
      f out
    ExitFailure i ->
      let errm = L.unlines [
              "Process exited with failing status: " <> T.unpack (renderIntegral i)
            , err
            ]
      in counterexample errm (property False)

tmpDirProp :: (FilePath -> IO Property) -> Property
tmpDirProp f =
  testIO (withTempDirectory "./dist/" "gen-XXXXXX" f)

-- -----------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
