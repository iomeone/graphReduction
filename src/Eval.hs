module Eval where 


import Types
import State


eval :: TiState -> [TiState]
eval state = state : restStates
    where 
        restStates | tiFinal state = []
                   | otherwise = eval nextState
        nextState = incStep (step state)


incStep :: TiState -> TiState
incStep = applyToSteps tiStepInc


tiFinal :: TiState -> Bool
tiFinal ([isOnlyDataAddr], [], heap, gloabls, steps) = isValueNode (hLookup heap isOnlyDataaddr)
-- if current stack has only one data, comutation is done!
tiFinal ([]     , [], _, _, _)                       = error "Empty stack"
tiFinal _                                            = False


step :: TiState -> TiState






