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

import           System.Directory (createDirectoryIfMissing, makeAbsolute)
import           System.Exit (ExitCode(..))
import           System.FilePath.Posix ((</>), takeDirectory)
import           System.IO (FilePath, IO)
import           System.IO.Temp (withTempDirectory)
import           System.Process (readProcessWithExitCode)

import           Test.Machinator.Core.Arbitrary


-- -----------------------------------------------------------------------------

typesPropV1 :: Property
typesPropV1 =
  -- N.B. this is only testing one file right now
  gamble genDefinitionFileV1 $ \(Versioned _ df) ->
    either (\e -> counterexample (T.unpack (renderHaskellTypesError e)) (property False)) id $ do
      fts <- MH.typesV1 [df]
      pure (conjoin (fmap (uncurry ghcProp) fts))

-- -----------------------------------------------------------------------------

-- Compiles with GHC in the current sandbox, failing if exit status is nonzero.
ghcProp :: FilePath -> Text -> Property
ghcProp mname modl =
  fileProp mname modl
    (\path -> readProcessWithExitCode "cabal" ["exec", "--", "ghc", path] "")
    (processProp (const (property True)))

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


fileProp :: FilePath -> Text -> (FilePath -> IO a) -> (a -> Property) -> Property
fileProp mname modl f g =
  testIO . withTempDirectory "./dist/" "gen-XXXXXX" $ \tmpDir -> do
    let path = tmpDir </> mname
        dir = takeDirectory path
    createDirectoryIfMissing True dir
    T.writeFile path modl
    path' <- makeAbsolute path
    fmap g (f path')

-- -----------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
