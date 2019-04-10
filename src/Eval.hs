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
step state@(stack, _,heap, _, _) = dispatch (hLookup heap (head stack))
    where 
        dispatch (NNum n                    ) = numStep state n
        dispatch (NAp a1 a2                 ) = apStep state a1 a2
        dispatch(NSupercomb sc args body    ) = scStep state sc args body
        dispatch(NPrim name primitive       ) = primStep state primitive
        dispatch(NInd addr                  ) = scInd state addr
        dispatch(NData tag _                ) = dataStep state tag





dataStep :: TiState -> Integer -> TiState
dataStep (stack@(_:stackRest), dump, heap, globals, stats) tag = case  dump of
    d:ds ->(d ++ stackRest, ds, heap, globals, stats)
    -- dump a stack out
    -- question ???? stackRest always empty???



    _ -> error $"Data constructor applied as a function"
    -- if the state is not Final step and also NData , so the dump must have some stacks !