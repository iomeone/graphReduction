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
    d:ds | stackRest /= [] ->error "dataStep: why stackRest is not empty?"
    d:ds | stackRest == [] ->(d ++ stackRest, ds, heap, globals, stats)
    -- dump a stack out
    -- question ???? stackRest always empty???

    _ -> error $"Data constructor applied as a function" ++ show tag
    -- if the state is not Final step and also NData , so the dump must have some stacks !



numStep :: TiState -> Integer -> TiState
numStep (stack@(_ : stackRest), dump, heap, gloabls, steps) n = case dump of
    d:ds | stackRest /= [] -> error "numStep: why stackRest is not empty?"
    d:ds | stackRest == [] -> (d ++ stackRest, ds, heap, globals, steps)

    _  ->error $ "Number applied as a function" ++ show n



apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack@(top : _) , dump, heap, gloabls, steps) a1 a2 = 
    case hLookup heap a2 of
        NInd a3 -> (a1 : stack, dump, hUpdate heap top (NAp a1 a3), globals, steps)
        -- if a2 is an indirect computation, we convert it to the direct computation
        -- we put a1 to the stack because we now start the a1 computation.

        _ -> (a1: stack, dump, heap, gloabls, steps)
        -- we just simple compute the a1




scInd :: TiState -> Addr -> TiState
scInd (stack, dump, heap, gloabls, steps) addr = 
    (addr : tail stack, dump, heap, gloabls, steps)
    -- if current computation is an indirect computation, we just computate it's inner expr(addr points).


scStep :: TiState -> String -> [String] -> Expr -> TiState
scStep (stack@(stackTop : stackRest), dump, heap, globals, steps) sc argNames body
        =(stack1, dump, heap1, globals, steps)
        where 
            root = stack !! length argNames
            -- root is the foremost pos which the scNode(sc is super combination) occupied.
            -- for example , stack is  0:main 1:arg1 2:arg2 3:otherFun
            -- the root will be the address of 2

            stack1 = root : drop (length argNames + 1) stack
            --we drop the whole scNode occupied spaces (contain all arguments and the super combinator name itself)
            --we push the root address to the front of the stack
            --but for the moment, the root address point to the arbitary node. we will allocate the root address soon.
            
            heap1 = instantiateAndUpdate body heap env root
            -- we alloc the node the root addr point to.
            -- question: why we use the root addr?
            -- because the root addr is not used anymore, we simply reused the heap space!

            env = arg_bindings ++ globals
            -- update the env, the super combinator's argument is put in the front of the stack.
            
            stackArgs = getArgs heap stack

            arg_bindings | length stackArgs >= length argNames = zip argNames stackArgs
                         | otherwise = error $ "Not enough arguments for function " ++ sc





getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map (getArg heap) stack
getArgs _ [] = []


-- getArg :: Heap Node -> Addr -> Addr
-- getArg heap addr = arg 
--             where 
--                 (NAp _ arg) = hLookup heap addr

getArg :: Heap Node -> Addr -> Addr
getArg heap addr = 
    case hLookup heap addr of
        NAp func arg     -> arg
        NInd addr           -> getArg heap addr
        x                   -> error $ "getArg: NAp node is expected, but got " ++ show x 


