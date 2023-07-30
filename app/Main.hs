module Main where
import WordleGame

-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

-- Call the WordleGame with a value greater than 1 to get into the game mode
main :: IO [Int]
main = wordleGameMode 1
