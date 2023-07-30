module Main where
import WordleGame
import Test.HUnit
import qualified System.Exit as Exit
import Data.Text

strConv :: String -> Text
strConv s = pack s

textConv :: Text -> String
textConv s = unpack s

test1 :: Test
test1 =
 let (retVal, retBool) = getWordle (strConv "pig") (strConv "sit") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '0', 'm', 's', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '2', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '4', '0', 'm', 't', '\ESC', '[', '0', 'm']) (retVal))

test2 :: Test
test2 = let (retVal, retBool) = getWordle (strConv "pig") (strConv "pig") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '2', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '2', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '4', '2', 'm', 'g', '\ESC', '[', '0', 'm']) (retVal))

test3 :: Test
test3 = let (retVal, retBool) = getWordle (strConv "cat") (strConv "pig") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '0', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '0', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '4', '0', 'm', 'g', '\ESC', '[', '0', 'm']) (retVal))

test4 :: Test
test4 = let (retVal, retBool) = getWordle (strConv "cats") (strConv "pig") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '0', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '0', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '4', '0', 'm', 'g', '\ESC', '[', '0', 'm','\ESC', '[', '4', '0', 'm','s' ,'\ESC', '[', '0', 'm']) (retVal))

test5 :: Test
test5 = let (retVal, retBool) = getWordle (strConv "catp") (strConv "pig") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '0', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '0', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '4', '0', 'm', 'g', '\ESC', '[', '0', 'm','\ESC', '[', '4', '0', 'm','p' ,'\ESC', '[', '0', 'm']) (retVal))

test6 :: Test
test6 = let (retVal, retBool) = getWordle (strConv "sips") (strConv "pig") 
 in TestCase (assertEqual "should return" (strConv ['\ESC', '[', '4', '0', 'm', 'p', '\ESC', '[', '0', 'm', '\ESC', '[', '4', '2', 'm', 'i', '\ESC', '[', '0', 'm', '\ESC', '[',
 '1', '0','3', 'm', 'p', '\ESC', '[', '0', 'm','\ESC', '[', '4', '0', 'm','s' ,'\ESC', '[', '0', 'm']) (retVal))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3,
                  TestLabel "test4" test4,TestLabel "test5" test5,TestLabel "test6" test6]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

