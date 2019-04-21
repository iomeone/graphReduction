module Eval where 

import Debug.Trace



import Types
import State


eval :: TiState -> [TiState]
eval state = state : restStates
    where 
        -- restStates | tiFinal state = trace ("------------------------------------------is final node" ++ show state) []
        restStates | tiFinal state = []
                --    | otherwise = trace ("------------------------------------------not final node" ++ show state) $ eval nextState
                   | otherwise = eval nextState
        nextState = incStep (step state)


incStep :: TiState -> TiState
incStep = applyToSteps tiStepInc


tiFinal :: TiState -> Bool
tiFinal ([isOnlyDataAddr], [], heap, gloabls, steps) = isValueNode (hLookup heap isOnlyDataAddr)
-- if current stack has only one data, comutation is done!
tiFinal ([]     , [], _, _, _)                       = error "Empty stack"
tiFinal _                                            = False


step :: TiState -> TiState
step state@(stack, _,heap, _, _) = dispatch (hLookup heap (head stack))
    where 
        dispatch(NNum n                     ) = numStep state n
        dispatch(NAp a1 a2                  ) = apStep state a1 a2
        dispatch(NSupercomb sc args body    ) = scStep state sc args body
        dispatch(NPrim name primitive       ) = primStep state primitive
        dispatch(NInd addr                  ) = scInd state addr
        dispatch(NData tag _                ) = dataStep state tag





dataStep :: TiState -> Integer -> TiState
dataStep (stack@(_:stackRest), dump, heap, globals, stats) tag = case  dump of
    d:ds | stackRest /= [] ->error $ "dataStep: why stackRest is not empty?" ++ (show stack)   -- why stackRest is not empty? becasue the 
    d:ds | stackRest == [] ->(d ++ stackRest, ds, heap, globals, stats)
    -- dump a stack out
    -- question ???? stackRest always empty???

    -- d:ds  ->(d ++ stackRest, ds, heap, globals, stats)

    


   

    _ -> error $"Data constructor applied as a function" ++ show tag
    -- if the state is not Final step and also NData , so the dump must have some stacks !



numStep :: TiState -> Integer -> TiState
numStep (stack@(_ : stackRest), dump, heap, globals, steps) n = case dump of
    d:ds | stackRest /= [] -> error "numStep: why stackRest is not empty?"
    d:ds | stackRest == [] -> (d ++ stackRest, ds, heap, globals, steps)

    _  ->error $ "Number applied as a function" ++ show n



apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack@(top : _) , dump, heap, globals, steps) a1 a2 = 
    case hLookup heap a2 of
        NInd a3 -> (a1 : stack, dump, hUpdate heap top (NAp a1 a3), globals, steps)
        -- if a2 is an indirect computation, we convert it to the direct computation
        -- we put a1 to the stack because we now start the a1 computation.

        _ -> (a1: stack, dump, heap, globals, steps)
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



primStep :: TiState -> Primitive -> TiState
primStep state Add = primBinary state (fromBinary (+))



primStep state (Construct tag arity) = primConstruct state tag arity
primStep state If = primIf state
primStep state CasePair  = primCasePair state
primStep state CaseList  = primCaseList state

primStep state x = error $ "-----------error :" ++ show x



pairApply :: Heap Node -> Node -> Addr -> (Heap Node, Node)
pairApply heap (NData 2 [x, y]) f = (heap2 ,appNode)
  where 
    (heap1, addr ) = hAlloc heap (NAp f x)
    -- we add (NAp f x) node
    (heap2, addr1) = hAlloc heap1 (NAp addr y) 
    -- we add (NAp addr y) node

    appNode = hLookup heap2 addr1
    -- we get the Node we just allocate

pairApply _ _ _  = error "Function expects a pair"



primCasePair :: TiState -> TiState
primCasePair state@(stack@(caseAddr : p : f : stackRest), dump, heap, globals, stats) = state1
 where
    (pAddr, fAddr) = (getArg heap p, getArg heap f)
    pair           = hLookup heap pAddr
    state1
        | isPairNode pair
        = let (heap1, app) = pairApply heap pair fAddr
          in (f : stackRest, dump, hUpdate heap1 f app, globals, stats)
    
        | isDataNode pair
        = error "Expected a pair as argument to casePair"
        
        | otherwise
        = ([pAddr] ,stack : dump, heap, globals, stats)
        -- = ([pAddr],  (p : f : stackRest): dump, heap, globals, stats)


primCasePair _ = error "Malformed casePair-expression"        



listApply  :: Heap Node -> Node -> Addr -> Addr -> (Heap Node, Node)
listApply heap (NData 2 []) continueFunAddr initValAddr= 
    (heap, (NInd initValAddr))

    -- if the list node is a nil node, we just return the init value.
    -- the reason why we use NInd is that we will not compute the init value until we really need it.


listApply heap (NData 3 [headAddr, tailAddr]) continueFunAddr initValAddr = 
    (heap2, appNode)
    where 
        (heap1, firstApplyToHead) = hAlloc heap (NAp continueFunAddr headAddr)
        -- for example we compute
        -- length xs = caseList xs 0 length'
        -- length' x xs = 1 + length xs
        
        -- this line of code is do the : length' x 


        (heap2, sndApplyToTail)   = hAlloc heap1(NAp firstApplyToHead tailAddr)

        -- this line of code is do the : length' x xs


        appNode = hLookup heap2 sndApplyToTail

        -- we get the Node we just allocate







primCaseList state@(stack@(app_x_caseAddr : app_x_lstAddr : app_x_initValAddr : app_x_continueFunAddr : stackRest), dump, heap, gloabls, stats) 
    | length args < 3 = error "primCaseList: wrong number of args"
    | not (isValueNode listNode) = ([lstAddr], stack : dump, heap, gloabls, stats)
    | otherwise = (newStack, dump, updatedNewHeap, gloabls, stats)
        where 
            args = getArgs heap stack 

            (lstAddr : initValAddr : continueFunAddr : rest) = args

            listNode = hLookup heap lstAddr 

            newStack = drop 3 stack
            -- we drop the app_x_caseAddr app_x_lstAddr app_x_initValAddr and let app_x_continueFunAddr be the new root.

            root_of_redex = head newStack
            -- we get the root of redex's pointer

            NData tag compts = listNode
            -- current listNode is must be an NData node, because if it is not, we put it in the stack and compute it first.

            [headLst, remainLst] = compts

            (newHeap, newAppNode) = listApply heap listNode continueFunAddr initValAddr
            -- we convert the caselist function to continue function .
            -- for example: we convert length xs = caseList xs 0 length' to  length' x xs  
            -- note: length' x xs = 1 + length xs

            updatedNewHeap = hUpdate newHeap root_of_redex newAppNode
            -- we update the root_of_redex with newAppNode







    









primIf :: TiState -> TiState
primIf (stack@(_ : condition : trueApp : falseApp : restStack), dump, heap, gloabls, stats) = 
    let
        (condAddr, trueAddr, falseAddr) = 
            (getArg heap condition, getArg heap trueApp, getArg heap falseApp)
         
        conditionNode = hLookup heap condAddr
        
        state1
            | isTrueNodeEx heap condAddr
            -- =(trueApp : restStack, dump, hUpdate heap trueApp (NInd trueAddr) , gloabls, stats)
            -- i am so naive ! we should alway update the falseApp addr, bcz, falseApp is the key addr to update(the first stack in dump has some value should be update to NData)
            =trace "--------------------------------------------------------------------------------is True Node"
            (falseApp : restStack, dump, hUpdate heap falseApp (NInd trueAddr) , gloabls, stats)
            
            | isFalseNodeEx heap condAddr
            =trace "--------------------------------------------------------------------------------is False Node"
            (falseApp : restStack, dump, hUpdate heap falseApp (NInd falseAddr), gloabls, stats)


            | isValueNode conditionNode
            = error "number in if condition"

            | otherwise
            =trace "--------------------------------------------------------------------------------otherwise"
            ([condAddr], stack : dump, heap, gloabls, stats)
    in
        trace ("--------------------------------------------------------------------------------primIf addr:" ++ (show condAddr) ++ " " ++ (show $isTrueNodeEx heap condAddr) ++  " " ++ (show $isFalseNodeEx heap condAddr))
            state1


primConstruct :: TiState -> Integer -> Integer -> TiState
primConstruct (stack@(root : restStack), dump, heap, globals, steps) tag arity
    = let args = take (fromIntegral arity) (getArgs heap stack)
          stackNew = drop (length args) stack

          heap1 = hUpdate heap (head stackNew) (NData tag args)
       in (stackNew, dump, heap1, globals, steps)


primBinary :: TiState -> (Node -> Node -> Node) -> TiState
primBinary (stack@(_ : xRoot : yRoot : stackRest), dump, heap, globals, steps) f
        -- = trace "------------------------------------------primBinary" state1
    = state1
    where
        (xAddr, yAddr) = (getArg heap xRoot, getArg heap yRoot)
        (x    , y)     = (hLookup heap xAddr, hLookup heap yAddr)
        state1
            | isNumNode x && isNumNode y
            = let heap1 = hUpdate heap yRoot (f x y)
              in (yRoot : stackRest, dump, heap1, globals, steps)
              -- we update the node yRoot pointer. which is the result we compute just.


            | not (isNumNode x)
            = ([xAddr], [yRoot] : dump, heap, globals, steps)
              -- if xNode is not num, we put the xAddr in the new stack, and start another stack computation.
            | not (isNumNode y)
            =([yAddr], [yRoot] : dump, heap, globals, steps)
              -- if yNode is not num, we put the yAddr in the new stack, and start another stack computation.




instantiate
        :: Expr            -- Body of super combinator
        -> TiHeap          -- Heap before instantiation
        ->TiGlobals        -- Association list of names and adresses
        ->(TiHeap, Addr)   -- Heap after instantiation, and the addr of root node
instantiate (ENum n ) heap _ = hAlloc heap (NNum n)

instantiate(EAp e1 e2) heap env = 
    hAlloc heap2 (NAp a1 a2)
    where
        (heap1, a1) = instantiate e1 heap  env
        (heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v) heap env = 
    (heap, aLookup env v (error ("Undefined name " ++ show v)))



instantiateAndUpdate :: Expr -> TiHeap -> TiGlobals -> Addr -> TiHeap
instantiateAndUpdate (ENum n) heap _ addr = hUpdate heap addr (NNum n)

instantiateAndUpdate (EVar v) heap env addr = 
    hUpdate heap addr (NInd (aLookup env v (error ("Undefined name " ++ show v))))


instantiateAndUpdate (EAp e1 e2) heap env addr = 
    let (heap1, a1) = instantiate e1 heap env
        (heap2, a2) = instantiate e2 heap1 env
    in hUpdate heap2 addr (NAp a1 a2)



instantiateAndUpdate (EConst tag arity) heap _ addr =
  hUpdate heap addr (NPrim "Pack" (Construct tag arity))



fromBinary :: (Integer -> Integer -> Integer) -> Node -> Node -> Node
fromBinary f (NNum x) (NNum y) = NNum $ f x y




getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map (getArg heap) stack
getArgs _ [] = []


-- getArg_ :: Heap Node -> Addr -> Addr
-- getArg_ heap addr = 
--     case hLookup heap addr of
--         NAp func arg     -> arg
--         NInd addr           -> getArg heap addr
--         x                   -> error $ "getArg: NAp node is expected, but got " ++ (show x )

getArg :: Heap Node -> Addr -> Addr
getArg heap addr = 
    case hLookup heap addr of
        NAp func arg     -> arg
        -- NInd addr           -> getArg heap addr
        x                   -> error $ "getArg: NAp node is expected, but got " ++ (show x )


