module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.Process (callCommand)
import Text.Printf (printf)

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
      exampleFile = "data/examples/" ++ dayPadded ++ "-1.txt"

  createModuleFile moduleFile dayModule day
  createEmptyFile exampleFile
  updateCabalFile dayModule
  updateTestSpec dayModule
  updateDayRunner dayModule day
  updateSubmitRunner dayModule day

  putStrLn "\nDownloading input..."
  callCommand $ printf "cabal run download %d" day

  putStrLn "---"
  putStrLn $ "🎄 Type `cabal run day " ++ show day ++ "` to run your solution."
  putStrLn $ "   Add your example inputs to data/examples/" ++ dayPadded ++ "-1.txt, " ++ dayPadded ++ "-2.txt, etc."

createModuleFile :: FilePath -> String -> Int -> IO ()
createModuleFile path moduleName day = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Module file \"" ++ path ++ "\" already exists"
    else do
      let content =
            T.pack $
              unlines
                [ "module " ++ moduleName ++ " (solution, part1, part2, tests) where",
                  "",
                  "import AoC.Template (Day (..), readExample, solve)",
                  "import qualified Data.Text as T",
                  "import Test.Hspec",
                  "",
                  "day :: Day",
                  "day = Day " ++ show day,
                  "",
                  "part1 :: T.Text -> Maybe Int",
                  "part1 _ = Nothing",
                  "",
                  "part2 :: T.Text -> Maybe Int",
                  "part2 _ = Nothing",
                  "",
                  "solution :: IO ()",
                  "solution = solve day part1 part2",
                  "",
                  "-- Tests",
                  "tests :: Spec",
                  "tests = describe \"" ++ moduleName ++ "\" $ do",
                  "  it \"solves part 1 with example 1\" $ do",
                  "    input <- readExample day 1",
                  "    part1 input `shouldBe` Just 0 -- TODO: Replace with expected value",
                  "",
                  "  -- Uncomment to test with additional examples (create files like " ++ printf "%02d" day ++ "-2.txt, " ++ printf "%02d" day ++ "-3.txt, etc.)",
                  "  -- it \"solves part 1 with example 2\" $ do",
                  "  --   input <- readExample day 2",
                  "  --   part1 input `shouldBe` Just 0",
                  "",
                  "  it \"solves part 2 with example 1\" $ do",
                  "    input <- readExample day 1",
                  "    part2 input `shouldBe` Just 0 -- TODO: Replace with expected value"
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

updateDayRunner :: String -> Int -> IO ()
updateDayRunner moduleName day = do
  let dayFile = "app/Day.hs"
  exists <- doesFileExist dayFile
  if not exists
    then putStrLn $ "Day runner \"" ++ dayFile ++ "\" not found; skipping registration"
    else do
      dayContent <- TIO.readFile dayFile
      let importLine = T.pack $ "import qualified " ++ moduleName
          caseLine = T.pack $ "runDay " ++ show day ++ " = " ++ moduleName ++ ".solution"
          hasImport = T.isInfixOf importLine dayContent
          hasCase = T.isInfixOf caseLine dayContent

      if hasImport && hasCase
        then putStrLn "Day runner already up to date"
        else do
          let withImport =
                if not hasImport
                  then addDayImport dayContent moduleName
                  else dayContent
              withCase =
                if not hasCase
                  then addDayCase withImport moduleName day
                  else withImport
          TIO.writeFile dayFile withCase
          putStrLn $ "Added " ++ moduleName ++ " to day runner"

addDayImport :: T.Text -> String -> T.Text
addDayImport content moduleName =
  T.replace
    (T.pack "import Text.Printf (printf)")
    (T.pack $ "import Text.Printf (printf)\nimport qualified " ++ moduleName)
    content

addDayCase :: T.Text -> String -> Int -> T.Text
addDayCase content moduleName day =
  T.replace
    (T.pack "runDay day = do")
    (T.pack $ "runDay " ++ show day ++ " = " ++ moduleName ++ ".solution\nrunDay day = do")
    content

updateSubmitRunner :: String -> Int -> IO ()
updateSubmitRunner moduleName day = do
  let submitFile = "app/Submit.hs"
  exists <- doesFileExist submitFile
  if not exists
    then putStrLn $ "Submit runner \"" ++ submitFile ++ "\" not found; skipping registration"
    else do
      submitContent <- TIO.readFile submitFile
      let importLine = T.pack $ "import qualified " ++ moduleName
          case1 = T.pack $ "runDay " ++ show day ++ " 1 = return . " ++ moduleName ++ ".part1"
          case2 = T.pack $ "runDay " ++ show day ++ " 2 = return . " ++ moduleName ++ ".part2"
          hasImport = T.isInfixOf importLine submitContent
          hasCase1 = T.isInfixOf case1 submitContent
          hasCase2 = T.isInfixOf case2 submitContent

      if hasImport && hasCase1 && hasCase2
        then putStrLn "Submit runner already up to date"
        else do
          let withImport =
                if not hasImport
                  then addSubmitImport submitContent moduleName
                  else submitContent
              withCases =
                if not hasCase1 && not hasCase2
                  then addSubmitCases withImport moduleName day
                  else withImport
          TIO.writeFile submitFile withCases
          putStrLn $ "Added " ++ moduleName ++ " to submit runner"

addSubmitImport :: T.Text -> String -> T.Text
addSubmitImport content moduleName =
  T.replace
    (T.pack "-- AUTOGEN-IMPORTS (scaffold will add day imports here)")
    (T.pack $ "-- AUTOGEN-IMPORTS (scaffold will add day imports here)\nimport qualified " ++ moduleName)
    content

addSubmitCases :: T.Text -> String -> Int -> T.Text
addSubmitCases content moduleName day =
  T.replace
    (T.pack "-- AUTOGEN-CASES (scaffold will add day cases here)")
    (T.pack $ "-- AUTOGEN-CASES (scaffold will add day cases here)\nrunDay " ++ show day ++ " 1 = return . " ++ moduleName ++ ".part1\nrunDay " ++ show day ++ " 2 = return . " ++ moduleName ++ ".part2")
    content

updateTestSpec :: String -> IO ()
updateTestSpec moduleName = do
  let specPath = "test/Spec.hs"
  exists <- doesFileExist specPath
  if not exists
    then putStrLn $ "Spec file \"" ++ specPath ++ "\" not found; skipping test registration"
    else do
      specContent <- TIO.readFile specPath
      let importLine = T.pack $ "import qualified " ++ moduleName
          hasImport = T.isInfixOf importLine specContent
          hasTest = T.isInfixOf (T.pack $ moduleName ++ ".tests") specContent

      if hasImport && hasTest
        then putStrLn "Spec already up to date"
        else do
          let withImport =
                if not hasImport
                  then T.replace (T.pack "-- AUTOGEN-IMPORTS") (T.pack $ "-- AUTOGEN-IMPORTS\nimport qualified " ++ moduleName) specContent
                  else specContent
              withTest =
                if not hasTest
                  then T.replace (T.pack "-- AUTOGEN-TESTS") (T.pack $ "-- AUTOGEN-TESTS\n    " ++ moduleName ++ ".tests") withImport
                  else withImport
          TIO.writeFile specPath withTest
          putStrLn $ "Added " ++ moduleName ++ " to test/Spec.hs"
