module Main (main) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import Control.Exception (catch, SomeException)

year :: Int
year = 2025

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr] -> downloadInput (read dayStr)
    _ -> die "Usage: cabal run download <day>"

downloadInput :: Int -> IO ()
downloadInput day = do
  sessionCookie <- BS.readFile "session.cookie"
  let url = printf "https://adventofcode.com/%d/day/%d/input" year day
      outputFile = printf "data/inputs/%02d.txt" day
  
  request <- parseRequest url
  let request' = setRequestHeader (CI.mk $ BS.pack "Cookie") [BS.pack "session=" <> BS.strip sessionCookie] request
  
  catch (do
    response <- httpBS request'
    BS.writeFile outputFile (getResponseBody response)
    putStrLn $ "Downloaded input for day " ++ show day ++ " to " ++ outputFile
    ) (\e -> do
      let err = show (e :: SomeException)
      die $ "Failed to download input: " ++ err ++ "\n\nMake sure session.cookie exists with your AoC session token."
    )
