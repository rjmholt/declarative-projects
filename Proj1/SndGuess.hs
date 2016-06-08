import Proj1
import Data.List

firstGuess = fst initialGuess

putAssoc :: Eq a => (a,b) -> [(a,[b])] -> [(a,[b])]
putAssoc pair = put pair []
  where
    put :: Eq a => (a,b) -> [(a,[b])] -> [(a,[b])] -> [(a,[b])]
    put (k,v) seen [] = (k,[v]):seen
    put (k,v) seen ((kL,vs):assocs)
      | k == kL    = seen ++ ((kL,v:vs):assocs)
      | otherwise = put (k,v) ((kL,vs):seen) assocs

pairWithResponse :: (Chord -> (Int,Int,Int)) -> Chord -> ((Int,Int,Int),Chord)
pairWithResponse f chord = (f chord, chord)

mapThrough :: (a -> b) -> (c,a) -> (c,b)
mapThrough f (x,y) = (x,f y)

targsWithResponse = foldr (putAssoc . pairWithResponse (`chordResponse` firstGuess)) [] initState

nextBestGuess = map (mapThrough $ pickBestGuess' initState) targsWithResponse

main = mapM_ print nextBestGuess
