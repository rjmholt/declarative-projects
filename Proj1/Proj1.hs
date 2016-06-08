{-# OPTIONS_GHC -O3 -fllvm #-}

module Proj1 (Chord, GameState, initialGuess, nextGuess) where

import Control.Parallel.Strategies
import Data.List
import Responder (chordResponse)

type Chord = [String]
type GameState = [Chord]

-- List all possible chords (1330 in total).
-- b < a and c < b used since order does not matter; eliminates repeats
initState :: GameState
initState = [[a,b,c] | a <- pitches, b <- pitches, c <- pitches, b<a, c<b]
  where
    pitches = [x:[y] | x <- ['A'..'G'], y <- ['1'..'3']]

-- For two variables x and y, compares f(x) and f(y) and returns the
-- variable whose mapping succeeds in the comparison
compareMap :: Ord a => (a -> a -> Bool) -> (b -> a) -> b -> b -> b
compareMap compare f x y = if f x `compare` f y then x else y

-- Determine whether a possible guess would match the response received previously
isCandidate :: (Int,Int,Int) -> Chord -> Chord -> Bool
isCandidate (pitch,note,oct) guess poss = chordResponse guess poss == (pitch,note,oct)

-- Finds the number of possible targets that will remain if a guess is
-- used against a given answer
numEliminated :: [Chord] -> Chord -> Chord -> Int
numEliminated remTargs guess targ = length $ filter eliminated remTargs
  where
    eliminated = not . isCandidate (chordResponse targ guess) guess

-- Scores a guess based on the minimum number of possible targets it is
-- guaranteed to eliminate; find the number of possibilities a guess will
-- eliminate for every possible target and score it based on the minimum
-- of those
guessScore :: [Chord] -> Chord -> Int
guessScore remTargs guess = minimum possScores'
  where
    possScores = map (numEliminated remTargs guess) remTargs
    possScores' = possScores `using` parList rdeepseq

-- Find the guess with the maximum score (which has the highest minimum
-- number of targets eliminated).
pickBestGuess :: (Int,Int,Int) -> GameState -> GameState -> Chord
pickBestGuess resp allChords possTargets
  | length possTargets <= 2 = head possTargets
  | otherwise               = foldl1' (compareMap (>) (guessScore possTargets)) allChords
--  | length possTargets > 60 = secondGuess resp

-- Choose an initial guess given by "pickBestGuess' initState initState"
-- (pickBestGuess' is defined in the note below)
initialGuess :: (Chord, GameState)
initialGuess = (["A2","B2","C3"], initState)

-- Precalculated optimum second guess for given response
-- The second guess was too long, so has been hard coded.
secondGuess :: (Int,Int,Int) -> Chord
secondGuess (r,n,o) =
  case r of
    0 -> case n of
      0 -> case o of
        0 -> ["G3","G2","F3"]
        1 -> ["G3","F3","E1"]
        2 -> ["G3","F2","E1"]
        3 -> ["G3","G2","F1"]
      1 -> case o of
        0 -> ["G3","F1","C3"]
        1 -> ["G1","C2","B1"]
        2 -> ["G3","F3","B3"]
        3 -> ["G3","F3","E2"]
      2 -> case o of
        0 -> ["G3","F3","C1"]
        1 -> ["G3","B3","A1"]
        2 -> ["G1","C2","B2"]
        3 -> ["G3","F3","B3"]
      3 -> case o of
        0 -> ["C1","B1","A1"]
        1 -> ["G3","G1","B3"]
        2 -> ["G3","G2","B3"]
    1 -> case n of
      0 -> case o of
        0 -> ["G3","F1","C3"]
        1 -> ["G3","F3","B2"] 
        2 -> ["G3","F3","B2"]
      1 -> case o of
        0 -> ["G3","F3","B1"]
        1 -> ["G3","B3","A2"]
        2 -> ["G3","B2","A1"]
      2 -> case o of
        0 -> ["G3","F3","B1"]
        1 -> ["G3","G1","B2"]
        2 -> ["C2","B2","A3"]
    2 -> case n of
      0 -> case o of
        0 -> ["G2","C3","B1"]
        1 -> ["G3","F3","C2"]
      1 -> case o of
        0 -> ["G3","C1","B2"]
    3 -> ["C3","B2","A2"]

{- == NOTE ==
| The values for this function were calculated with the
| following code:
| 
|>  firstGuess = fst initialGuess
|> 
|>  putAssoc :: Eq a => (a,b) -> [(a,[b])] -> [(a,[b])]
|>  putAssoc pair = put pair []
|>    where
|>      put :: Eq a => (a,b) -> [(a,[b])] -> [(a,[b])] -> [(a,[b])]
|>      put (k,v) seen [] = (k,[v]):seen
|>      put (k,v) seen ((kL,vs):assocs)
|>        | k == kL    = seen ++ ((kL,v:vs):assocs)
|>        | otherwise = put (k,v) ((kL,vs):seen) assocs
|> 
|>  pairWithResponse :: (Chord -> (Int,Int,Int)) -> Chord -> ((Int,Int,Int),Chord)
|>  pairWithResponse f chord = (f chord, chord)
|> 
|>  mapThrough :: (a -> b) -> (c,a) -> (c,b)
|>  mapThrough f (x,y) = (x,f y)
|> 
|>  pickBestGuess' :: GameState -> GameState -> Chord
|>  pickBestGuess' allChords possTargets
|>    | length possTargets <= 2 = head possTargets
|>    | otherwise               = foldl1' (compareMap (>) (guessScore possTargets)) allChords
|>
|>  targsWithResponse = foldr (putAssoc . pairWithResponse (`chordResponse` firstGuess)) [] initState
|> 
|>  nextBestGuess = map (mapThrough $ pickBestGuess' initState) targsWithResponse
|> 
|>  main = mapM_ print nextBestGuess
-}

-- Make the next guess by taking the first matching chord in the list of possible
-- chords, determined by filtering out all chords that do not match the response
-- for the previous guess
nextGuess :: (Chord, GameState) -> (Int,Int,Int) -> (Chord, GameState)
nextGuess (guess, prevState) (pitch,note,oct) = (newGuess, nextState)
  where
    newGuess = pickBestGuess (pitch,note,oct) initState nextState
    nextState = filter (isCandidate (pitch,note,oct) guess) prevState

