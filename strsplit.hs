-- Tommy Li (tommyli@me.com)
-- 2010-12-08

-- This is a Haskell version of Guy Steele's "String Split" in Fortress
-- His talk was presented at YOW Australia (Melbourne) 2010 conference
-- I saw a tweet someone did it in Python (https://gist.github.com/724905) so I decided to try it in Haskell

data WordState = Chunk String | Segment String [String] String
  deriving Show

addWordState :: WordState -> WordState -> WordState
addWordState (Chunk str1) (Chunk str2) = Chunk (str1 ++ str2)
addWordState (Chunk str1) (Segment strl xs strr) = Segment (str1 ++ strl) xs strr
addWordState (Segment strl1 xs1 strr1) (Chunk str2) = Segment strl1 xs1 (strr1 ++ str2)
addWordState (Segment strl1 xs1 strr1) (Segment strl2 xs2 strr2) =
  Segment strl1 (xs1 ++ (maybeWord (strr1 ++ strl2)) ++ xs2) strr2

maybeWord :: String -> [String]
maybeWord [] = []
maybeWord str = [str]

processChar :: Char -> WordState
processChar ' ' = Segment "" [] ""
processChar char = Chunk [char]

splitStr :: String -> [String]
splitStr str =
  let
    wordList = map processChar str
    wordState = foldr addWordState (Chunk "") wordList
  in
    wordStateToList wordState

wordStateToList :: WordState -> [String]
wordStateToList (Chunk str) = [str]
wordStateToList (Segment strl xs strr) = strl:xs ++ [strr]

testStr = "Here is a sesquipedalian string of words"
-- assert ["Here","is","a","sesquipedalian","string","of","words"] (splitStr testStr)
