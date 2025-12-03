module Main (main) where

import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)

-- AUTOGEN-IMPORTS (scaffold will add day imports here)

readYear :: IO Int
readYear = read <$> readFile ".aoc-year"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayStr, partStr, answer] -> submitSolution (read dayStr) (read partStr) answer
    [dayStr, partStr] -> do
      let day = read dayStr :: Int
          part = read partStr :: Int
      answer <- computeAnswer day part
      submitSolution day part answer
    _ -> die "Usage: cabal run submit <day> <part> [answer]"

computeAnswer :: Int -> Int -> IO String
computeAnswer day part = do
  input <- TIO.readFile $ printf "data/inputs/%02d.txt" day
  result <- runDay day part input
  case result of
    Just answer -> return $ show answer
    Nothing -> die "No solution computed (returned Nothing)"

runDay :: Int -> Int -> T.Text -> IO (Maybe Int)
-- AUTOGEN-CASES (scaffold will add day cases here)
runDay day _ _ = die $ "Day " ++ show day ++ " not yet implemented"

submitSolution :: Int -> Int -> String -> IO ()
submitSolution day part answer = do
  year <- readYear
  sessionCookie <- BS.readFile "session.cookie"
  let url = printf "https://adventofcode.com/%d/day/%d/answer" year day
      body = LBS.pack $ "level=" ++ show part ++ "&answer=" ++ answer
  
  request <- parseRequest url
  let request' = setRequestMethod (BS.pack "POST")
               $ setRequestHeader (CI.mk $ BS.pack "Cookie") [BS.pack "session=" <> BS.strip sessionCookie]
               $ setRequestHeader (CI.mk $ BS.pack "Content-Type") [BS.pack "application/x-www-form-urlencoded"]
               $ setRequestBodyLBS body
               $ request
  
  catch (do
    response <- httpBS request'
    let responseBody = BS.unpack $ getResponseBody response

    if "That's the right answer" `isInfixOf` responseBody
      then putStrLn "✓ Correct answer!"
      else if "That's not the right answer" `isInfixOf` responseBody
        then putStrLn "✗ Wrong answer"
        else if "You gave an answer too recently" `isInfixOf` responseBody
          then putStrLn "⏳ Please wait before submitting again"
          else if "Did you already complete it" `isInfixOf` responseBody
            then putStrLn "Already completed"
            else putStrLn $ "Response: " ++ take 200 responseBody
    ) (\e -> do
      let err = show (e :: SomeException)
      die $ "Failed to submit: " ++ err
    )
