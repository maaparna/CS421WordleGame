{-# LANGUAGE OverloadedStrings #-}
module WordleGame where

import Control.Monad (join)
import Data.Text
  ( Text,
    pack,
    strip,
    toLower,
    unpack,
    length
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import System.Random (getStdGen, randomR)
import Data.List ( intersect )
import Data.String (String)
import Text.ParserCombinators.ReadPrec (reset)
import System.Exit ( exitFailure )

--Total number of attempts allowed per round
attempts :: Int
attempts = 6

--The main function that compares the entered word with the selected word.
--It marks the letters that are in the correct place with green background
--It marks the letters that is in incorrect place within the word length with yellow background
--It marks the letters that are not part of the correct word and the letters
--that are more than the word length with black background

--The function returns a Text object (word with the color background) and a bool flag 
--to indicate if the word attempted matches the correct word

--The coloring is done by the go function in the where clause which is a recursive function.
--It loops through each letter of the attempted word to determine the background colour

getWordle :: Text -> Text -> (Text, Bool)
getWordle attempt correct =
  let sAttempt= unpack attempt
      sCorrect= unpack correct
      result = go sAttempt sCorrect (sAttempt `intersect` sCorrect)
      tResult = pack result
      isCorrect = attempt == correct
   in (tResult , isCorrect)
  where
    go :: String -> String -> String -> String
    go [] [] _ = []
    go [] (u:us) intersection = '\ESC':'[':'4':'0':'m':u:'\ESC':'[':'0':'m' : go [] us intersection
    go (w:ws) [] intersection = '\ESC':'[':'4':'0':'m':w:'\ESC':'[':'0':'m' : go ws [] intersection
    go (w:ws) (u:us) intersection
      | w == u = '\ESC':'[':'4':'2':'m':w:'\ESC':'[':'0':'m': go ws us intersection
      | w `elem` intersection = '\ESC':'[':'1':'0':'3':'m':w:'\ESC':'[':'0':'m': go ws us intersection
      | otherwise = '\ESC':'[':'4':'0':'m':u:'\ESC':'[':'0':'m': go ws us intersection

--These are SCC (Set Cost Center) to help with performance profiling of the getWordle function
{-# SCC getWordle #-}


-- This is a recursive function that allos the user to guess the word to the max of "attempts" times
--It calls the getWordle function for each entry and prints out the word with the colored background to
--proviude hint to the user

wordleGame :: Text ->Int-> IO [Text]
wordleGame selected_word word_length = go attempts []
  where
    go :: Int -> [Text] -> IO [Text]
    go 0 xs = do
        putStrLn "Better luck next time"
        printf "The correct word is %s \n you have used %d/%d attempts\n\n" selected_word attempts attempts
        return xs
    go n xs = do
      let i = 1 + Prelude.length xs

      attemptstr <- getLine
      let attemp = toLower . strip $ pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "you have used %d/%d attempts\n\n" i attempts

      TIO.putStrLn wordle
      printf "\n\n"

      if correct
        then do
          putStrLn "Congratulations!!"
          printf "The correct word is %s and you have guessed %s \n you have used %d/%d attempts\n\n" selected_word wordle i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

--These are SCC (Set Cost Center) to help with performance profiling of the wordleGame function
{-# SCC wordleGame #-}

--This function uses a random number generator to pick a ord from the file list and use that as the 
--word for the user to guess
--Once the word is selected a prompt is provided for the user to enter a word of the expected length
--The selected word and its length are sent as arguments to the wordleGame function to start the counter on user attempts
getWord :: String -> IO ()
getWord filename = do
    wordleEText <- readFile filename
    let wordleGEText =
            join
            . map T.words
            . T.lines
            . toLower
            $ pack wordleEText

    g <- getStdGen
    let selected_indexGE = fst $ randomR (0 :: Int, Prelude.length wordleGEText) g
    let selected_wordGE = wordleGEText !! selected_indexGE
    let wordGELength= Data.Text.length selected_wordGE

    putStrLn $ "Please enter your " ++ show wordGELength ++ " letters: "
    wordlesGE <- wordleGame selected_wordGE wordGELength

    TIO.putStrLn . T.unlines . reverse $ wordlesGE

--These are SCC (Set Cost Center) to help with performance profiling of the getWord function
{-# SCC getWord #-}


--the filename for the easy mode
easyWordleGameMode:: IO ()
easyWordleGameMode =
  let filename = "Easy.txt" in getWord filename

--These are SCC (Set Cost Center) to help with performance profiling of the easyWordleGameMode function
{-# SCC easyWordleGameMode #-}

--the filename for the medium mode
mediumWordleGameMode:: IO ()
mediumWordleGameMode =
  let filename = "Medium.txt" in getWord filename

--These are SCC (Set Cost Center) to help with performance profiling of the mediumWordleGameMode function
{-# SCC mediumWordleGameMode #-}


--the filename for the hard mode
hardWordleGameMode:: IO ()
hardWordleGameMode = let filename = "Hard.txt" in getWord filename

--These are SCC (Set Cost Center) to help with performance profiling of the hardWordleGameMode function
{-# SCC hardWordleGameMode #-}

--This if the initial entry function for the game
--the user is given 4 choices to choose from and based on the entry will be taken through the specific mode
--If the user enters choice 4 it calls the WordleGameMode function with a number 0 which will make it exit
wordleGameMode :: Int -> IO [Int]
wordleGameMode num = do
  if num > 0 then do
    putStrLn "Enter 1 to enter easy mode"
    putStrLn "Enter 2 to enter medium mode"
    putStrLn "Enter 3 to enter difficult mode"
    putStrLn "Enter 4 to Exit"

    userChoice <- getLine

    if userChoice == "1"
      then do
        num <- easyWordleGameMode
        printf "\n\n"
        wordleGameMode 1
    else
        if userChoice == "2"
            then do
                num <- mediumWordleGameMode
                printf "\n\n"
                wordleGameMode 1
        else
          if userChoice == "3"
            then do

                num <- hardWordleGameMode
                printf "\n\n"
                wordleGameMode 1

          else
             if userChoice == "4"
              then do
                 wordleGameMode 0
             else do
               putStrLn "\ESC[1mPlease provide a valid input!!"
               putStrLn "\ESC[0m"
               wordleGameMode 1
  else
     exitFailure

--These are SCC (Set Cost Center) to help with performance profiling of the wordleGameMode function
{-# SCC wordleGameMode #-}