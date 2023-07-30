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
--import System.Console.ANSI

attempts :: Int
attempts = 6

-- strConv :: String -> Text
-- strConv s = pack s

-- textConv :: Text -> String
-- textConv s = unpack s

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
{-# SCC getWordle #-}
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

      --putStrLn $ "Please enter your " ++ show word_length ++ " letters: "

      attemptstr <- getLine
      let attemp = toLower . strip $ pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "you have used %d/%d attempts\n\n" i attempts
    --   printf "you \n\n"

    --   let w = strConv ['\ESC', '[', '4', '0', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '2', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
    --           '4', '0', 'm', 'g', '\ESC', '[', '0', 'm']

    --   TIO.putStrLn w

    --   printf "you \n\n"


      --TIO.putStrLn . T.unlines . reverse $ (wordle : xs)

      TIO.putStrLn wordle
      printf "\n\n"

      if correct
        then do
          putStrLn "Congratulations!!"
          printf "The correct word is %s and you have guessed %s \n you have used %d/%d attempts\n\n" selected_word wordle i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)
{-# SCC wordleGame #-}

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
{-# SCC getWord #-}



easyWordleGameMode:: IO ()
easyWordleGameMode =
  let filename = "Easy.txt" in getWord filename
{-# SCC easyWordleGameMode #-}

mediumWordleGameMode:: IO ()
mediumWordleGameMode =
  let filename = "Medium.txt" in getWord filename
{-# SCC mediumWordleGameMode #-}

hardWordleGameMode:: IO ()
hardWordleGameMode = let filename = "Hard.txt" in getWord filename
{-# SCC hardWordleGameMode #-}


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
{-# SCC wordleGameMode #-}