------------------------------------------------------------------------
-- Homework 1 CIS 352, Spring 2015, Version 2 
-- Authors: Pedro de Oliveira Lira
------------------------------------------------------------------------
import Data.Char       
import Data.List       (nub)
import Test.QuickCheck
import Text.Printf hiding (toChar)
------------------------------------------------------------------------
pleaseFix = error "Please fix me!!"

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 1: Time conversions

convert :: Int -> (Int,Int,Int)
convert 0 = (0,0,0)
convert t
        | t < 60 = (0,0,t)
        | otherwise = (h,m,s)
        where
          (h,m,s) = (t `div` 3600, (t `mod`3600)`div`60 , ((t `mod`3600)`mod`60))
          
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 2: Testing for Vowels

-- isVowel ch = tests if ch is a lowercase vowel
--   Use `elem` (http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#v:elem)
isVowel :: Char -> Bool
isVowel ch = ch `elem` ['a','e','i','o','u']

-- based on http://learnyouahaskell.com/starting-out#im-a-list-comprehension

        
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 3: Disemvoweling 

-- disEmvowel str = the result of removing all vowels from str
disEmvowel :: String -> String
disEmvowel str =  [ c | c <- str, c `notElem` ['a','e','i','o','u']]

--- based on http://learnyouahaskell.com/starting-out#im-a-list-comprehension

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 4: 

-- smash str 
--   = the result of removing all non-letters from str and
--     changing all uppercase letters to lowercase.
-- E.g.,  smash "Fee, Fie, Foe, and Fum!" 
--     returns: "feefiefoefum"
-- Use a helper function if you want
smash :: String -> String
smash "" = ""
smash str = [ toLower(x) | x <- str, isAlpha(x)]                  


------------------------------------------------------------------------
------------------------------------------------------------------------
-- toNum 'a' = 0, toNum 'b' = 1, ...., toNum 'z' = 25, toNum ch = -1 o/w.
toNum :: Char -> Int
toNum ch = if ('a'<=ch && ch <= 'z') then ord ch - ord 'a' else -1

------------------------------------------------------------------------
-- toChar 0 = 'a', toChar 1 = 'b', ...., toNum 25 = 'z', toNum n = '?' o/w.
toChar :: Int -> Char
toChar n = if (0<= n && n <= 25) then chr (n+ord 'a') else '?'

------------------------------------------------------------------------
-- Problem 5: Circular Shift Cipher

-- shift n str = the result of circularly shifting the letters in 
--               (smash str)  n places.
-- E.g.,  shift 1  "abcxyz" returns "bcdyza"
-- E.g.,  shift 0  "abcxyz" returns "abcxyz"
-- E.g.,  shift 25 "abcxyz" returns "zabwxy"
-- You can use toNum, toChar, and mod. (There are other ways.)
-- Also use a helper function if you want.

-- function to smash and transform a word in number
smashToNum :: String -> [Int] 
smashToNum str = [toNum(word) | word <- smash(str)]
-- function to add shift value to the word in number
shiftToNum :: Int -> String -> [Int] 
shiftToNum n str = [ x+n | x <- smashToNum(str)]
-- function to format the words/numbers on limit cases
helperFormat :: Int -> Int 
helperFormat x
     | x > 25 || x < 0 = x `mod` 26
     | otherwise = x

shift :: Int -> String -> String
shift n "" =  ""
shift 0 str = smash(str)
shift n str = [toChar(intWord) | intWord <- [helperFormat(word) | word <- shiftToNum n str]]

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Problem 6: Capitization 

capitalized :: String -> String
capitalized "" = ""
capitalized (c:cs) = toUpper(c):[toLower(x) | x <- cs]

------------------------------------------------------------------------
-- Problem 7: Title

--- Helper function to capitalize words
--- if len(word) > 4 or return it unmodified  
helper :: String -> String
helper (str)
        | length str > 4 = capitalized str
        | otherwise = str

title :: [String] -> [String]
title [] = []
title (first:rest) = capitalized(first):[helper(x) | x <- rest]


-----------------------------------------------------------------------
-- Tests created by student

-- These are the tests made by me. I decided to use the
-- whole output structure that already existed on the
-- homework1 file. That way, I created these test cases
-- and their output is print when quickCheck runs.
-- Although they are 'quickCheck'tests, they are not
-- automatically made.

convert_myTest1 = convert (0) == (0,0,0)
convert_myTest2 = convert (59) == (0,0,59)
convert_myTest3 = convert (120) == (0,2,0)

vowel_myTest1 = isVowel(' ') == False
vowel_myTest2 = isVowel('!') == False
vowel_myTest3 = (isVowel('e') == True) && (isVowel('E') == False)

disemv_myTest1 = disEmvowel("aeiou") == ""
disemv_myTest2 = disEmvowel("") == ""
disemv_myTest3 = disEmvowel("babaca") == "bbc"

smash_myTest1 = smash("            ") == ""
smash_myTest2 = smash("The Castle of Wolfstein") == ("thecastleofwolfstein")
smash_myTest3 = smash("135126436!!!!!") == ""

shift_myTest1 = shift 1 ("") == ""
shift_myTest2 = shift (-1) ("aaaa") == ("zzzz")
shift_myTest3 = shift 1 ("zzzz") == ("aaaa")

cap_myTest1 = capitalized("") == ""
cap_myTest2 = capitalized("the") == "The"
cap_myTest3 = capitalized("hUGEWORDSTOGETHERINONE") == "Hugewordstogetherinone"

title_myTest1 = title([""]) == [""]
title_myTest2 = title(["the","small","title"]) == ["The","Small","Title"]
title_myTest3 = title(["the","not","so","big","title"]) == ["The","not","so","big","Title"]

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
-- test properties


convert_prop t = (h*60+m)*60+s == (abs t)
                 && 0 <= h && 0 <= m && 0 <= s
                 && m < 60 && s < 60
    where
      (h,m,s) = convert (abs t)


-- nonsense words
--    Try: sample (arbitrary :: Gen Wrd)
newtype Wrd = Wrd String deriving (Show)
instance Arbitrary Wrd
    where arbitrary = 
              do txt <- listOf $
                        frequency [(678,return 'a'), (123,return 'b'), 
                                   (230,return 'c'), (352,return 'd'), 
                                   (1055,return 'e'), (189,return 'f'),
                                   (167,return 'g'), (506,return 'h'), 
                                   (577,return 'i'), (12,return 'j'), 
                                   (63,return 'k'), (334,return 'l'),
                                   (200,return 'm'), (560,return 'n'), 
                                   (624,return 'o'), (160,return 'p'), 
                                   (7,return 'q'),   (495,return 'r'), 
                                   (524,return 's'), (752,return 't'),
                                   (230,return 'u'), (81,return 'v'), 
                                   (195,return 'w'), (12,return 'x'), 
                                   (164,return 'y'), (5,return 'z')]
                 return (Wrd txt)

-- nonsense words with a trailing space or punctuation
--    Try: sample (arbitrary :: Gen Wrd2)
newtype Wrd2 = Wrd2 String deriving (Show)
instance Arbitrary Wrd2
    where arbitrary = do (Wrd txt) <- arbitrary
                         if (null txt) 
                         then return (Wrd2 "")
                         else do trm <- (frequency [(7, return " "),
                                                    (1, return ", "),
                                                    (1, return ". "),
                                                    (1, return "! "),
                                                    (1, return "? ")])
                                 return (Wrd2 (txt++trm))

-- a string of nonsense words separated by spaces
--    Try: sample (arbitrary :: Gen Blither)
newtype Blither = Blither String deriving (Show)
instance Arbitrary Blither 
    where arbitrary = 
              do wrds <- listOf arbitrary 
                 return (Blither (concat [cs++" " | (Wrd cs)<-wrds, 
                                                    not (null cs)]))

-- a string of nonsense words separated by spaces and punctuation
--    Try: sample (arbitrary :: Gen Twaddle)
newtype Twaddle = Twaddle String deriving (Show)
instance Arbitrary Twaddle
    where arbitrary = 
              do wrds <- listOf arbitrary 
                 return (Twaddle (concat [cs | (Wrd2 cs) <- wrds,
                                               not (null cs) ]))
vowel_prop c = isVowel c == (p (ord c - ord 'a') == 0)
    where 
      p x = x*(8960 + x*(-4448 + x*(720 + x*(-46 + x))))

disemv_prop (Twaddle cs) = disEmvowel cs == filter (`notElem` "aeiou") cs

isAsciiLetter c = isAscii c && isLetter c

smash_prop (Twaddle cs) = smash cs == concatMap foo cs
    where 
      foo c = if isAsciiLetter c then (toLower c:[]) else []

shift1_prop key
    = length (nub (shift key "abcdefghijklmnopqrstuvwxyz")) == 26

shift2_prop key (Twaddle mess)
    = (shift (-key) (shift key mess)) == smash mess

isCap wrd = null wrd || (isUpper (head wrd) && all isLower (tail wrd))

cap_prop (Wrd str) =  isCap (capitalized str')
    where 
      str' = filter isAsciiLetter str

title_prop (Blither cs) 
    = ws /= [] ==> isCap (head ws) && all ok (tail ws)
    where
        ws = title $ filter (not.null) $ map (filter isAsciiLetter) (words cs)
        ok wrd
            | length wrd <= 4 = all isLower wrd
            | otherwise       = isCap wrd
               
------------------------------------------------------------------------

tests :: [(String, IO ())]
tests = [ ("convert prop",     quickCheck convert_prop)
        , ("convert myTest1",   quickCheck convert_myTest1)
        , ("convert myTest2",   quickCheck convert_myTest2)
        , ("convert myTest3",   quickCheck convert_myTest3)
        , ("vowel prop",       quickCheck vowel_prop)
        , ("vowel myTest1",     quickCheck vowel_myTest1)
        , ("vowel myTest2",     quickCheck vowel_myTest2)
        , ("vowel myTest3",     quickCheck vowel_myTest3)  
        , ("disemvowel prop",  quickCheck disemv_prop)
        , ("disemvowel myTest1", quickCheck disemv_myTest1)
        , ("disemvowel myTest2", quickCheck disemv_myTest2)
        , ("disemvowel myTest3", quickCheck disemv_myTest3)
        , ("smash prop",       quickCheck smash_prop)
        , ("smash myTest1",       quickCheck smash_myTest1)
        , ("smash myTest2",       quickCheck smash_myTest2)
        , ("smash myTest3",       quickCheck smash_myTest3)
        , ("shift prop 1",     quickCheck shift1_prop)  
        , ("shift prop 2",     quickCheck shift2_prop)
        , ("shift myTest1",     quickCheck shift_myTest1)  
        , ("shift myTest2",     quickCheck shift_myTest2)  
        , ("shift myTest3",     quickCheck shift_myTest3)  
        , ("capitalized prop", quickCheck cap_prop)
        , ("capitalized myTest1", quickCheck cap_myTest1)
        , ("capitalized myTest2", quickCheck cap_myTest2)
        , ("capitalized myTest3", quickCheck cap_myTest3)
        , ("title prop",       quickCheck title_prop)
        , ("title myTest1",       quickCheck title_myTest1)
        , ("title myTest2",       quickCheck title_myTest2)
        , ("title myTest3",       quickCheck title_myTest3)
        ]


testRun = mapM_ (\(s,a) -> (printf "%-25s: " s) >> a) tests

