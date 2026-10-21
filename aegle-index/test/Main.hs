module Main (main) where

import Aegle.Prelude
import Aegle.Core.Name qualified as TS
import Aegle.Database.Backend qualified as DB
import Aegle.Index qualified as Index
import Control.Foldl qualified as Foldl
import Data.Set qualified as S
import Hedgehog
import Paths_aegle_index (getDataFileName)
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Aegle.Index"
  [ testProperty "reexports refer to indexed definitions" $
      withTests 1 $ property do
        fixture <- evalIO $ getDataFileName "test/fixtures/reexports"
        fragments <- evalIO $
          Index.index (config fixture) silentLogger (Foldl.generalize Foldl.list)
        let definitions = allDefinitions fragments
            exports = exportsFrom "Reexports" fragments
            indexedNames = S.fromList $ map (\d -> d.name) definitions
            exportedNames = map (.exportAs.name) exports

        -- ordinary reexports survive
        assert $ "zero" `elem` exportedNames
        assert $ "suc" `elem` exportedNames
        -- erased reexports disappear
        assert $ all (`notElem` exportedNames) ["lzero", "lsuc", "_⊔_", "Setω"]
        -- remaining reexports have an indexed target
        assert $ all
          (\e -> e.canonicalName `S.member` indexedNames)
          exports
  ]
  where
    config fixture = Index.Config [Index.LibraryConfig Index.None fixture]

    silentLogger :: Index.Logger
    silentLogger _ _ = pure ()

    allDefinitions :: [DB.LibraryFragment] -> [DB.Definition]
    allDefinitions = concatMap (.definitions)

    exportsFrom :: TS.ModuleName -> [DB.LibraryFragment] -> [DB.Export]
    exportsFrom name fragments =
      [ e
      | f <- fragments
      , e <- f.exports
      , e.exportAs.moduleName == name
      ]
