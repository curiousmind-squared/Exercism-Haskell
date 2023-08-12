{-
Introduction
Bob is a lackadaisical teenager. He likes to think that he's very cool. And he definitely doesn't get excited about things. That wouldn't be cool.
When people talk to him, his responses are pretty limited.
Instructions
Your task is to determine what Bob will reply to someone when they say something to him or ask him a question.
Bob only ever answers one of five things:
    "Sure." This is his response if you ask him a question, such as "How are you?" The convention used for questions is that it ends with a question mark.
    "Whoa, chill out!" This is his answer if you YELL AT HIM. The convention used for yelling is ALL CAPITAL LETTERS.
    "Calm down, I know what I'm doing!" This is what he says if you yell a question at him.
    "Fine. Be that way!" This is how he responds to silence. The convention used for silence is nothing, or various combinations of whitespace characters.
    "Whatever." This is what he answers to anything else.
You need to implement the responseFor function that returns Bob's response for a given input.
-}


-- Está bem complicado entender o que o exercício quer que façamos 
import Data.Char
responseFor :: String -> String
responseFor st 
    | checkEmpty st == True = "Fine. Be that way!"
    | last st == '?' && checkUpper st == True = "Calm down, I know what I'm doing!"
    | checkUpper st == True = "Whoa, chill out!"
    | (last $ (removeOnlySpaces st)) == '?' = "Sure."
    | otherwise = "Whatever."

-- lasts :: String -> String
-- lasts st = last (init st):last st:[]

checkUpper :: String -> Bool
checkUpper st = (all (isUpper) $ removeUselessShit st) && not (all (isSpace) $ removeUselessShit st)

removeOnlySpaces :: String -> String
removeOnlySpaces st = [c | c <- st, c /= ' ']

removeUselessShit :: String -> String
removeUselessShit st = [c | c <- st, c /= ' ', not (c `elem` ['!', '?', '%', '^', '*', '@', ',', '#', '$', '(', '*', '^']), not (c `elem` ['0'..'9'])]

removeUselessShitbutNotNumbers :: String -> String
removeUselessShitbutNotNumbers st = [c | c <- st, c /= ' ', c /= '/', c /= 't', not (c `elem` ['!', '?', '%', '^', '*', '@', ',', '#', '$', '(', '*', '^'])]


checkEmpty :: String -> Bool
checkEmpty st = all (isSpace) $ removeUselessShitbutNotNumbers st 

