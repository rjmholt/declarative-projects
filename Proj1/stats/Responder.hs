module Responder (chordResponse) where

import Data.List

type GameState = [Chord]
type Chord = [Pitch]
type Pitch = String

-- List all possible chords (1330 in total).
-- b < a and c < b used since order does not matter; eliminates repeats
initState :: GameState
initState = [[a,b,c] | a <- pitches, b <- pitches, c <- pitches, b<a, c<b]
  where
    pitches = [x:[y] | x <- ['A'..'G'], y <- ['1'..'3']]

-- Get the response that would be generated by the test program
-- for a given correct answer (ans) and guess
chordResponse :: Chord -> Chord -> (Int,Int,Int)
chordResponse ans guess = (numRight, numNotes, numOctaves)
  where
    (numRight, ans', guess') = countDivide guess (0,ans,[])
    lenGuess = length guess'
    numNotes = lenGuess - length (reduceOnElem 0 ans' guess')
    numOctaves = lenGuess - length (reduceOnElem 1 ans' guess')

-- Removes the first occurrence in y where y's nth element
-- matches the nth element of any occurrence in x.
reduceOnElem :: Eq a => Int -> [[a]] -> [[a]] -> [[a]]
reduceOnElem n = foldr (deleteBy (eqNth n))
  where
    eqNth n x y = x!!n == y!!n
{-
- == NOTE ==
- I originally spent a lot of time rewriting this functionality for
- my program, both for efficiency and originality (as below):
- > countNotesOcts :: Pitch -> Pitch -> Chord -> (Int,Int) -> (Int,Int)
- > countNotesOcts _ _ [] (n,m) = (n,m)
- > countNotesOcts ansNotes ansOcts (g:gs) (n,m) = countNotesOcts ansNotes' ansOcts' gs (n',m')
- >   where
- >     (ansNotes',n') = getFirstBy (==g!!0) ansNotes [] n
- >     (ansOcts',m') = getFirstBy (==g!!1) ansOcts [] m
- > 
- > getFirstBy :: (a -> Bool) -> [a] -> [a] -> Int -> ([a],Int)
- > getFirstBy _ [] passed n = (passed, n)
- > getFirstBy f (x:xs) passed n = if f x then (passed++xs,n+1) else getFirstBy f xs (x:passed) n
- However, this turned out slower (I tried INLINE pragmas too) than folding deleteBy, losing
- out by about 0.5s (mapping all chords to each other). This time was needed to meet the 5s limit.
-}

-- Count the unique common elements between a given list and
-- another list ((x:xs) and lefts). Elements only in (x:xs)
-- are sorted into rights, while elements only in lefts remain
-- in lefts. This is essentially set dissection for symmetric
-- difference, but counts common elements without having to
-- call 'length' on another list
countDivide :: Eq a => [a] -> (Int,[a],[a]) -> (Int,[a],[a])
countDivide [] (n,lefts,rights) = (n,lefts,rights)
countDivide (x:xs) (n,lefts,rights) = countDivide xs (n',lefts',rights')
  where
    (found,lefts') = elemCheckDelete x lefts (False,[])
    (n',rights') = if found then (n+1,rights) else (n,x:rights)

-- Run over a list checking whether a given value is an element
-- Return a boolean reflecting whether it is in the list, and the
-- list with all occurences of it removed:
-- Returns (el `elem` xs, filter (/= el) xs)
elemCheckDelete :: Eq a => a -> [a] -> (Bool,[a]) -> (Bool,[a])
elemCheckDelete _ [] (inList, notMatches) = (inList, notMatches)
elemCheckDelete el (x:xs) (inList, notMatches)
  | el == x   = elemCheckDelete el xs (True, notMatches)
  | otherwise = elemCheckDelete el xs (inList, x:notMatches)

---- | Compute the correct answer to a guess.  First argument is the 
----   target, second is the guess.
--response :: [String] -> [String] -> (Int,Int,Int)
--response target guess = (right, rightNote, rightOctave)
--  where guess'      = nub guess
--        right       = length $ intersect guess' target
--        num         = length guess'
--        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
--                    - right
--        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
--                    - right
