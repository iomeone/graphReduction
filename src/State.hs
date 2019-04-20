module State where

import Debug.Trace

import Common
import Types

type Addr = Integer

data Node = 
    NAp Addr Addr 
    -- arg1: the operator addr
    -- arg2: ddr the operand addr

    | NSupercomb String [String] Expr  
    -- arg1: the function name 
    -- arg2: the list of function args
    -- arg3: the body of the function

    | NNum Integer
    -- arg1: an integer

    | NInd Addr 
    -- an indirect computation

    | NPrim String Primitive 
    -- arg1: primitive name
    -- arg2: the Primitive type, which define you can find in this file
    
    | NData Integer [Addr]
    -- arg1: the tag
    -- arg2: do not known so far!
    deriving Show

data Primitive = 
    Neg
    | Add
    | Sub
    | Mul
    | Div
    | Construct Integer Integer
    | If

    | CasePair
    deriving Show


extraPreludeDefs :: Program
extraPreludeDefs = 
    [
        ( "and"
        , ["x", "y"]
        , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
      , ( "or"
        , ["x", "y"]
        , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
      
      , ( "not"
        , ["x"]
        , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "False")) (EVar "True")
        )

      , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
      
      , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))

      , ("True", [],  EConst 0 0)

      , ("False", [], EConst 1 0)


      , ("Nil", [],   EConst 2 0)
      , ("Cons", [],  EConst 3 2)
        
    
    ]


primitives :: [ (String , Primitive)]
primitives = 
    [("negate",     Neg)
    ,("+",          Add)
    ,("-",          Sub)
    ,("*",          Mul)
    ,("/",          Div)
    ,("if",         If)


    ,("casePair", CasePair)
    -- ,("True"    , Construct 0 0)
    -- ,("False"   , Construct 1 0)
    ,("MkPair"  , Construct 2 2)
    ]

isValueNode :: Node -> Bool
isValueNode node = 
    isDataNode node || isNumNode node
    

isDataNode :: Node -> Bool
isDataNode (NData _ _) = True
isDataNode _           = False


isNumNode :: Node -> Bool
isNumNode (NNum _) = True
isNumNode _        = False



isAppNodeSimple :: Node -> Bool
isAppNodeSimple (NAp _ _) = True
isAppNodeSimple _         = False

isLambdaNodeSimple :: Node -> Bool
isLambdaNodeSimple (NSupercomb _ _ _ ) = True;
isLambdaNodeSimple _ = False;

showLambdaNodeSimple :: Node -> String
showLambdaNodeSimple  (NSupercomb name args _ ) = name ++ " " ++ (joinBy " " args )
showLambdaNodeSimple  n  = show n

isPrimNodeSimple :: Node -> Bool
isPrimNodeSimple (NPrim _ _) = True
isPrimNodeSimple _           = False

isValueNodeSimple :: Node -> Bool
isValueNodeSimple node = 
    isDataNodeSimple node || isNumNodeSimple node


isDataNodeSimple :: Node -> Bool
isDataNodeSimple (NData _ _) = True
isDataNodeSimple _           = False


isNumNodeSimple :: Node -> Bool
isNumNodeSimple (NNum _) = True
isNumNodeSimple _        = False
-- isTrueNode :: Node -> Bool
-- isTrueNode (NData 0 []) = True
-- isTrueNode _            = False

isTrueNodeEx :: TiHeap-> Addr -> Bool
isTrueNodeEx heap addr =   
    case (hLookup heap addr) of
        (NData 0 [])     -> True
        NInd addrind     -> isTrueNodeEx heap addrind
        _             ->False


isFalseNodeEx :: TiHeap-> Addr -> Bool
isFalseNodeEx heap addr =   
    case (hLookup heap addr) of
        (NData 1 [])     -> True
        NInd addrind     -> isFalseNodeEx heap addrind
        _             ->False

-- isFalseNode :: Node -> Bool
-- isFalseNode (NData 1 []) = True
-- isFalseNode _            = False


isPairNode (NData 2 [_, _]) = True
isPairNode _                = False

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiSteps)

type TiStack = [Addr]               --the current going on computation 

type TiDump = [TiStack]             -- the suspend computation


initialTiDump :: TiDump
initialTiDump = []


type TiHeap = Heap Node 

type TiGlobals = Assoc String Addr

data Heap a = Heap Integer [Integer] [(Addr, a)]
-- arg1: the current size of current heap
-- arg2: the free addresses can be used by new coming node.
-- arg3: the addess and the Node pair, each item represent what it's address of a node.

instance Show a => Show (Heap a) where 
    show (Heap size frees binds) = "\nHeap size:" ++ (show size) ++ " \nHeap nodes:\n" ++ (unlines  $map show binds)  ++ "[(Addr, a)]:\n"
-- this line do not show the free address, because  the free addresses are infinite array. will print forever.



hInitial :: Heap a
hInitial = Heap 0 [1 ..] []
--arg1: in the beginning, the size is zero.
--arg2: the free addresses are 1 to infinite.
--arg3: there is no Node at the begining.


hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x = 
    (Heap (size + 1) free ((next, x) : xs), next)
-- 1, we increase the size by 1
-- 2, we remove the used  free space, 
-- 3, add the new node and the new address to the heap's (node,addr) list
-- 4, we return the modified heap and the new node's addr


hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Not enough memory."
-- this seems will never happen.



hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) addr node = 
    Heap size free ((addr, node) : remove xs addr)



-- arg1: the heap's (addr, node) list.
-- arg2: the addr of node to remove.
-- return: the removed result result.   
remove :: [(Integer, a)] -> Integer -> [(Integer, a)]
remove ((addr, node) : xs) addrToRemove | addr == addrToRemove = xs
                                        | otherwise = (addr, node) : remove xs addrToRemove

-- get all the used heap address
hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ xs) = [addr | (addr, _) <- xs]



type Assoc a b = [(a, b)]
--arg1: the assoc list we are going to looking for.
--arg2: the key we are looking for
--arg3: default value, if we can not find the key, we return the default value.
        -- if you pass error "xxxx" , it's also right, because error return whatever type you want
--return: the value we find.
aLookup :: (Eq k) => Assoc k v -> k -> v ->v
aLookup [] _ def = def
aLookup ((k', v) :as) k def | k == k' = v
                            | otherwise = aLookup as k def


aLookUpValueNode :: (Eq k) => Assoc k Node -> k -> Node ->  Maybe Node
aLookUpValueNode [] _ def = Just def

aLookUpValueNode((k', v) : as) k def | k == k' = if (isValueNodeSimple v) then Just v else Nothing
                                     | otherwise = aLookUpValueNode as k def

-- isValueNode :: Node -> Bool
-- isValueNode node
isValueNode2 :: Node -> Maybe Node
isValueNode2 v= 
    if (isValueNode v) then Just v else Nothing

hLookup :: Heap a -> Addr -> a
hLookup (Heap _ _ xs) a = 
    aLookup xs a (error ("Heap.hLookup - can't find address " ++ show a))





data TiSteps = TiSteps Integer deriving Show


-- we set step to 0 at the begining
tiStepInitial :: TiSteps
tiStepInitial = TiSteps 0

-- inc the step count by 1
tiStepInc :: TiSteps -> TiSteps
tiStepInc (TiSteps n) = TiSteps (n + 1)

-- get the current step
tiStepGetStep :: TiSteps -> Integer
tiStepGetStep (TiSteps n) = n


--change the steps inside the TiState structure
applyToSteps :: (TiSteps -> TiSteps) -> TiState -> TiState
applyToSteps stepsFun (stack, dump, heap, scDefs, steps) = 
    (stack, dump, heap, scDefs, (stepsFun steps))


