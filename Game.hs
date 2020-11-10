--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

-- | This module should contain your game code.
module Game where
import Data.List

--------------------------------------------------------------------------------

-- | The number of pegs in a code.
pegs :: Int
pegs = 4

-- | Symbols are represented by characters.
type Symbol = Char

-- | The available symbols in the game.
symbols :: [Symbol]
symbols = ['a'..'f']

-- | A code is represented by a list of symbols.
type Code = [Symbol]

-- | Guesses are scored using coloured and white markers. The first component
-- of the pair gives the number of coloured markers and the right component
-- gives the number of white markers.
type Score = (Int, Int)

-- | A player is either human or computer-controlled.
data Player = Human | Computer

-- | The first codemaker in a play session.
codemaker :: Player
codemaker = Human

-- | The first guess the AI will make.
firstGuess :: Code
firstGuess = "aabb"

--------------------------------------------------------------------------------

-- | Determines whether a score indicates that a guess was correct or not.
------------------------------------
-- The guess is correct if every peg is the right colour and in the right place
-- so if the number of black markers is equal to the number of pegs and there are
-- consequently no white markers

correctGuess :: Score -> Bool
correctGuess score = score == (pegs,0)

-- | This function should check that the code entered by a human player is
-- valid. In other words, it should have the length given by `pegs` and it
-- should only contain valid symbols.
------------------------------------
-- The validateCode function checks if the length of the code is equal
-- to the number of pegs and uses a second function to validate the symbols
-- in the code

validateCode :: Code -> Bool
validateCode xs
      |length xs == pegs && validatechar xs = True
      |otherwise = False

-- This function uses recursion to checks
-- each symbol in the code individually to see if it is part of the list
-- symbols and only returns true if all of them are

validatechar ::   Code -> Bool
validatechar [] = True
validatechar (x:xs)
      | charFound x symbols && validatechar xs = True
      | otherwise = False

-- This is used to search each symbol in the list by traversing the list
-- until it was found. If the list finishes before it is found it returns
-- false

charFound :: Char -> String -> Bool
charFound c (x:xs)
      | c==x = True
      | xs =="" = False
      | otherwise = charFound c xs

-- | All possible codes.
------------------------------------
-- this is a list comprehension that calculates the cartesian product
-- symbols X symbols X symbols X symbols. It represents all possible combinations
-- of the characters in the symbol list made with only 4 characters. It is however
-- dependent on the number of pegs in the game, because if the number
-- changes this part will no longer work.

codes :: [Code]
codes = [[a,b,c,d] | a<- symbols,b<-symbols,c<-symbols,d<-symbols]

-- | All possible scores.
------------------------------------
-- The list comprehension generates all possible scores; for a score to be valis
-- the total number of white + black markers can't exceed the number of pegs.
--  Additionally, if all pegs except from one are in the right place, the
-- latter cannot be of the right colour but in the wrong place, so the
-- last condition prevents that case

results :: [Score]
results = [ (a,b) | a<-[0..pegs] , b<-[0..pegs], a+b<=pegs ,(a,b)/=(pegs-1,1)]

-- | Scores a guess against a code. Symbols which are in the right place
-- and of the right type score a coloured marker. Symbols which are of the
-- right type but in the wrong place score a white marker.
------------------------------------
-- The score function uses two additional functions, to determine
-- 1. The number of black pegs - adds 1 to the overall sum everytime symbols
-- on the same position in the code and guess are the same, else just keeps
-- looking through the rest of both lists simultaneously
-- 2. The number of white pegs - it uses a second function with an additional
-- parameter that will be used to calculate the number of whites. First the
-- number is set to be the total of common symbols between the code and the guess
-- (the total of black and white markers). Then it goes through the symbols in
-- both the code and guess and everytime it finds a black one (x = y) it decreases
-- the whites number by 1.

score :: Code -> Code -> Score
score code guess = (black code guess , white code guess)

black :: Code -> Code -> Int
black [] [] = 0
black (x:xs) (y:ys)
    | x==y = 1 + black xs ys
    | otherwise = black xs ys

white :: Code -> Code -> Int
white code guess = white' code guess listDifference
  where listDifference= pegs - length(code \\ guess)
white' [] [] whites = whites
white' (x:xs) (y:ys) a
        | x == y =  white' xs ys a-1
        |otherwise = white' xs ys a


-- | Chooses the next guess. If there is only one option left, choose it.
-- Otherwise, calculate the hit score for each code and choose the code
-- with the largest hit score.
------------------------------------

-- First the function calculates the length of the set for each score in
-- results against code. To obtain the hit score,it substracts the maximum
--  value in that list from the length of the list s.

hitscore :: Code -> [Code] -> Int
hitscore code s = length s - maximum [length ( eliminate x code s ) | x <- results]

-- The lists used in this function are
-- 1. max that calculates the maximum from all hit scores of each code in codes
-- 2. code that uses a list comprehension to generate all the codes that have
-- the maximum hit score calculated in max
-- 3. possibleCode that generates every code with the maximum hit score that
-- is also in s. As long as there still are possible codes, the next guess is
-- the head of the list. If there are no more elements in possibleCode then
-- use the head of the codes list.

nextGuess :: [Code] -> Code
nextGuess [x] = x
nextGuess s
        | length possibleCode > 1 = head possibleCode
        | otherwise = head code
        where possibleCode = [x | x <- code , elem x s ]
              code = [x |x <- codes , hitscore x s == max]
              max = maximum [hitscore x s | x <- codes]


-- | Remove all codes from the remaining possibilities which would result in
-- a different score for the guess if they were the code.
-- In other words, given the set of remaining possible codes, narrow it down
-- to those which would produce the same score we got from the codemaker.
------------------------------------
-- For every code in codes, checks if the score against the guess is the same
-- as the score between the guess and the code
eliminate :: Score -> Code -> [Code] -> [Code]
eliminate lastScore guess codes = [x | x <- codes, score guess x == lastScore]

--------------------------------------------------------------------------------
