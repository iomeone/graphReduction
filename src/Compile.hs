module Compile where


import Data.List    (mapAccumL)
-- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
import Types

import State



compile :: Program -> TiState
compile program = 
    ([], initialStack, initialTiDump, initialHeap, globals, tiStepInitial)
    where
        initialStack = [addressOfMain]
        (initialHeap, globals) = buildInitialHeap scDefs
        addressOfMain = aLookup globals "main" (error "main is not defined")
        scDefs = program ++ preludeDefs ++ extraPreludeDefs



-- arg1: all the super combinator definitions
-- arg2: the returned heap add all the [(Node name, Node address)]
buildInitialHeap :: [ScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs = (heap1, sc_addrs ++ pri_addrs)
    where
        (heap, sc_addrs) = mapAccumL allocateSc hInitial scDefs
        (heap1, pri_addrs) = mapAccumL allocatePrim heap primitives


-- arg1: the heap allocate operated on
-- arg2: allocate super combinator define function (type ScDefn = (String, [String], Expr) ) function name, argument list, body expr
-- arg3: we return the new heap, and new allocated addr and coresponding node
allocateSc :: TiHeap -> ScDefn -> (TiHeap, (String, Addr))
allocateSc heap (name, args, body) = (heap1, (name, addr))
    where (heap1, addr) = hAlloc heap (NSupercomb name args body)




-- arg1: the heap allocate operated on
-- arg2: is the predefined primitives like as:
                                            -- primitives :: [(String, Primitive)]
                                            -- primitives =
                                            --   [ ("negate"  , Neg)
                                            --   , ("+"       , Add)
-- arg3: we return the new heap, and new allocated addr and coresponding node

allocatePrim :: TiHeap -> (String, Primitive) -> (TiHeap, (String, Addr))
allocatePrim heap (name, prim) = 
    let (heap1, addr) = hAlloc heap (NPrim name prim) in (heap1, (name, addr))




























