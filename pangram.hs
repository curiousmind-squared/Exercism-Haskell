{-
 Your task is to figure out if a sentence is a pangram.
A pangram is a sentence using every letter of the alphabet at least once. It is case insensitive, so it doesn't matter if a letter is lower-case (e.g. k) or upper-case (e.g. K).
For this exercise, a sentence is a pangram if it contains each of the 26 letters in the English alphabet.
-}

import Data.Char

isPangram :: String -> Bool
isPangram text
   | testPangram text == "" = True
   | otherwise = False


check :: String -> Char -> String
check alphabet c = if c `elem` alphabet then (filter (/=c) alphabet) else alphabet

testPangram st = foldl check ['a'..'z'] $ toLowerCase $ removeWhite st

removeWhite :: String -> String
removeWhite st = filter (/= ' ') st

toLowerCase :: String -> String
toLowerCase st = map toLower st
