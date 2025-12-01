module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
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
      mainFile = "app/Main" ++ dayPadded ++ ".hs"
      inputFile = "data/inputs/" ++ dayPadded ++ ".txt"
      exampleFile = "data/examples/" ++ dayPadded ++ ".txt"
  
  createModuleFile moduleFile dayModule day
  createMainFile mainFile dayModule
  createEmptyFile inputFile
  createEmptyFile exampleFile
  updateCabalFile dayModule dayPadded
  
  putStrLn "---"
  putStrLn $ "🎄 Type `cabal run day" ++ dayPadded ++ "` to run your solution."

createModuleFile :: FilePath -> String -> Int -> IO ()
createModuleFile path moduleName day = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Module file \"" ++ path ++ "\" already exists"
    else do
      let content = T.pack $ unlines
            [ "module " ++ moduleName ++ " (solution) where"
            , ""
            , "import AoC.Template (Day(..), solve)"
            , "import qualified Data.Text as T"
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
            ]
      TIO.writeFile path content
      putStrLn $ "Created module file \"" ++ path ++ "\""

createMainFile :: FilePath -> String -> IO ()
createMainFile path moduleName = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Main file \"" ++ path ++ "\" already exists"
    else do
      let content = T.pack $ unlines
            [ "module Main (main) where"
            , ""
            , "import qualified " ++ moduleName
            , ""
            , "main :: IO ()"
            , "main = " ++ moduleName ++ ".solution"
            ]
      TIO.writeFile path content
      putStrLn $ "Created main file \"" ++ path ++ "\""

createEmptyFile :: FilePath -> IO ()
createEmptyFile path = do
  exists <- doesFileExist path
  if exists
    then putStrLn $ "File \"" ++ path ++ "\" already exists"
    else do
      writeFile path ""
      putStrLn $ "Created empty file \"" ++ path ++ "\""

updateCabalFile :: String -> String -> IO ()
updateCabalFile moduleName dayPadded = do
  cabalContent <- TIO.readFile "advent-of-code-haskell.cabal"
  
  let hasModule = T.isInfixOf (T.pack moduleName) cabalContent
      hasExecutable = T.isInfixOf (T.pack $ "executable day" ++ dayPadded) cabalContent
  
  if hasModule && hasExecutable
    then putStrLn "Cabal file already up to date"
    else do
      let updatedContent = if not hasModule
            then addModuleToLibrary cabalContent moduleName
            else cabalContent
          finalContent = if not hasExecutable
            then addExecutable updatedContent dayPadded
            else updatedContent
      TIO.writeFile "advent-of-code-haskell.cabal" finalContent
      when (not hasModule) $ putStrLn $ "Added " ++ moduleName ++ " to library"
      when (not hasExecutable) $ putStrLn $ "Added executable day" ++ dayPadded

addModuleToLibrary :: T.Text -> String -> T.Text
addModuleToLibrary content moduleName =
  T.replace
    (T.pack "  exposed-modules: AoC.Template")
    (T.pack $ "  exposed-modules: AoC.Template\n                 , " ++ moduleName)
    content

addExecutable :: T.Text -> String -> T.Text
addExecutable content dayPadded =
  content <> T.pack (unlines
    [ ""
    , "executable day" ++ dayPadded
    , "  import: shared-properties"
    , "  main-is: Main" ++ dayPadded ++ ".hs"
    , "  hs-source-dirs: app"
    , "  build-depends: base ^>=4.21.0.0"
    , "               , advent-of-code-haskell"
    , "               , text"
    ])

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()
