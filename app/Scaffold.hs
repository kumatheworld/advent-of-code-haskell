module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callCommand)
import Text.Printf (printf)
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> scaffoldDay (read dayStr)
    _ -> die "Usage: cabal run scaffold <day>"

scaffoldDay :: Int -> IO ()
scaffoldDay day = do
  let dayPadded = printf "%02d" day :: String
      dayModule = "Day" ++ dayPadded
      moduleFile = "src/" ++ dayModule ++ ".hs"
      exampleFile = "data/examples/" ++ dayPadded ++ ".txt"
  
  createModuleFile moduleFile dayModule day
  createEmptyFile exampleFile
  updateCabalFile dayModule
  
  putStrLn "\nDownloading input..."
  callCommand $ printf "cabal run download %d" day
  
  putStrLn "---"
  putStrLn $ "🎄 Type `cabal run day " ++ show day ++ "` to run your solution."

createModuleFile :: FilePath -> String -> Int -> IO ()
createModuleFile path moduleName day = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Module file \"" ++ path ++ "\" already exists"
    else do
      let content = T.pack $ unlines
            [ "module " ++ moduleName ++ " (solution, part1, part2, tests) where"
            , ""
            , "import AoC.Template (Day(..), solve, readExample)"
            , "import qualified Data.Text as T"
            , "import Test.Hspec"
            , ""
            , "day :: Day"
            , "day = Day " ++ show day
            , ""
            , "part1 :: T.Text -> Maybe Int"
            , "part1 _input = Nothing"
            , ""
            , "part2 :: T.Text -> Maybe Int"
            , "part2 _input = Nothing"
            , ""
            , "solution :: IO ()"
            , "solution = solve day part1 part2"
            , ""
            , "-- Tests"
            , "tests :: Spec"
            , "tests = describe \"" ++ moduleName ++ "\" $ do"
            , "  it \"solves part 1 correctly\" $ do"
            , "    input <- readExample day"
            , "    part1 input `shouldBe` Just 0  -- TODO: Replace with expected value"
            , "  "
            , "  it \"solves part 2 correctly\" $ do"
            , "    input <- readExample day"
            , "    part2 input `shouldBe` Just 0  -- TODO: Replace with expected value"
            ]
      TIO.writeFile path content
      putStrLn $ "Created module file \"" ++ path ++ "\""

createEmptyFile :: FilePath -> IO ()
createEmptyFile path = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "File \"" ++ path ++ "\" already exists"
    else do
      writeFile path ""
      putStrLn $ "Created empty file \"" ++ path ++ "\""

updateCabalFile :: String -> IO ()
updateCabalFile moduleName = do
  cabalContent <- TIO.readFile "advent-of-code-haskell.cabal"
  
  let hasModule = T.isInfixOf (T.pack moduleName) cabalContent
  
  if hasModule
    then putStrLn "Cabal file already up to date"
    else do
      let updatedContent = addModuleToLibrary cabalContent moduleName
      TIO.writeFile "advent-of-code-haskell.cabal" updatedContent
      putStrLn $ "Added " ++ moduleName ++ " to library"

addModuleToLibrary :: T.Text -> String -> T.Text
addModuleToLibrary content moduleName =
  T.replace
    (T.pack "  exposed-modules: AoC.Template")
    (T.pack $ "  exposed-modules: AoC.Template\n                 , " ++ moduleName)
    content
