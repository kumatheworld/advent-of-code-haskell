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
    [dayStr, partStr, answer] -> submitSolution (read dayStr) (read partStr) answer
    _ -> die "Usage: cabal run submit <day> <part> <answer>"

submitSolution :: Int -> Int -> String -> IO ()
submitSolution day part answer = do
  sessionCookie <- BS.readFile "session.cookie"
  let url = printf "https://adventofcode.com/%d/day/%d/answer" year day
      body = BS.pack $ "level=" ++ show part ++ "&answer=" ++ answer
  
  request <- parseRequest url
  let request' = setRequestMethod "POST"
               $ setRequestHeader (CI.mk $ BS.pack "Cookie") [BS.pack "session=" <> BS.strip sessionCookie]
               $ setRequestHeader (CI.mk $ BS.pack "Content-Type") [BS.pack "application/x-www-form-urlencoded"]
               $ setRequestBodyBS body
               $ request
  
  catch (do
    response <- httpBS request'
    let responseBody = BS.unpack $ getResponseBody response
    
    if "That's the right answer" `elem` words responseBody
      then putStrLn "✓ Correct answer!"
      else if "That's not the right answer" `elem` words responseBody
        then putStrLn "✗ Wrong answer"
        else if "You gave an answer too recently" `elem` words responseBody
          then putStrLn "⏳ Please wait before submitting again"
          else if "Did you already complete it" `elem` words responseBody
            then putStrLn "Already completed"
            else putStrLn $ "Response: " ++ take 200 responseBody
    ) (\e -> do
      let err = show (e :: SomeException)
      die $ "Failed to submit: " ++ err
    )
