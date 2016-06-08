import Responder (initState,chordResponse,Chord)

allPairs = [(x,y) | x <- initState, y <- initState]

applyFuncTuple :: (a -> b -> c) -> (a,b) -> c
applyFuncTuple f (x,y) = f x y

main = mapM_ (print . (applyFuncTuple chordResponse)) allPairs
