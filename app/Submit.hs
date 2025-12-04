module Main (main) where

import Control.Exception (catch, SomeException)
import Control.Monad (unless)
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
        mainText = extractMainText responseBody

    if "That's the right answer" `isInfixOf` responseBody
      then putStrLn "[CORRECT] That's the right answer!"
      else if "That's not the right answer" `isInfixOf` responseBody
        then do
          putStrLn "[WRONG] Wrong answer"
          unless (null mainText) $ putStrLn $ "  " ++ mainText
        else if "You gave an answer too recently" `isInfixOf` responseBody
          then do
            putStrLn "[WAIT] Please wait before submitting again"
            unless (null mainText) $ putStrLn $ "  " ++ mainText
          else if "Did you already complete it" `isInfixOf` responseBody
            then putStrLn "[DONE] Already completed"
            else putStrLn $ "Response: " ++ take 200 responseBody
    ) (\e -> do
      let err = show (e :: SomeException)
      die $ "Failed to submit: " ++ err
    )

-- Extract the main message text from the HTML response
-- The message is in <article><p>...</p></article> tags
extractMainText :: String -> String
extractMainText html =
  maybe "" stripHtmlTags (findBetween "<article>" "</article>" html)

-- Find text between two markers
findBetween :: String -> String -> String -> Maybe String
findBetween start end str =
  case breakOn start str of
    Nothing -> Nothing
    Just (_, afterStart) ->
      let content = drop (length start) afterStart
      in case breakOn end content of
           Nothing -> Nothing
           Just (between, _) -> Just between

-- Find the first occurrence of a substring
breakOn :: String -> String -> Maybe (String, String)
breakOn _ [] = Nothing
breakOn needle haystack@(_:rest)
  | needle `isPrefixOf` haystack = Just ([], haystack)
  | otherwise = case breakOn needle rest of
                  Nothing -> Nothing
                  Just (before, after) -> Just (head haystack : before, after)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Simple HTML tag stripper - removes tags and decodes common entities
stripHtmlTags :: String -> String
stripHtmlTags = unwords . words . decodeEntities . stripTags
  where
    stripTags [] = []
    stripTags ('<':rest) = ' ' : stripTags (dropWhile (/= '>') rest)
    stripTags ('>':rest) = ' ' : stripTags rest
    stripTags (c:rest) = c : stripTags rest

    decodeEntities [] = []
    decodeEntities ('&':'#':'3':'9':';':rest) = '\'' : decodeEntities rest
    decodeEntities ('&':'a':'m':'p':';':rest) = '&' : decodeEntities rest
    decodeEntities ('&':'l':'t':';':rest) = '<' : decodeEntities rest
    decodeEntities ('&':'g':'t':';':rest) = '>' : decodeEntities rest
    decodeEntities ('&':'q':'u':'o':'t':';':rest) = '"' : decodeEntities rest
    decodeEntities (c:rest) = c : decodeEntities rest
