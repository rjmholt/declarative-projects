import Proj1
import Data.List

firstGuess = fst initialGuess

mapFstSnd :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapFstSnd f g (x,y) = (f x, g y)

targComp :: Chord -> (Chord,(Int,Int,Int))
targComp c = (c, chordResponse firstGuess c)

getPoss :: (Int,Int,Int) -> [Chord]
getPoss resp = filter (isCandidate resp firstGuess) initState

firstResponses = map targComp initState

sndPoss = sort $ map (mapFstSnd (chordResponse firstGuess) (pickBestGuess initState . getPoss)) firstResponses

main = mapM_ print sndPoss
