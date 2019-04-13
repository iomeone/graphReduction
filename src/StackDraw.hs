module StackDraw (drawStackEx) where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import Types
import Graph
import State

type Stack = ([Addr], [(Addr, Node)])

joinBy sep list = drop (length sep) $ concat $ map (\item -> sep ++ item) list


graphStackItems :: String ->Stack -> GraphBuilder ()
graphStackItems parent (stack@(addr : addrs), heapAssoc) = (do
    
    addrStr <- addNode (show addr) VVar

    graphNode addrStr (aLookup heapAssoc addr (error "graphStackItems：graphNode")) heapAssoc

    return addrStr)
    >>= (do
        if parent == "" 
        then \node -> return () 
        else \node -> addEdge parent node) 



-- data Node = 
--     NAp Addr Addr 
--     -- arg1: the operator addr
--     -- arg2: ddr the operand addr

--     | NSupercomb String [String] Expr  
--     -- arg1: the function name 
--     -- arg2: the list of function args
--     -- arg3: the body of the function

--     | NNum Integer
--     -- arg1: an integer

--     | NInd Addr 
--     -- an indirect computation

--     | NPrim String Primitive 
--     -- arg1: primitive name
--     -- arg2: the Primitive type, which define you can find in this file
    
--     | NData Integer [Addr]
--     -- arg1: the tag
--     -- arg2: do not known so far!
--     deriving Show
graphNode :: String -> Node -> [(Addr, Node)] -> GraphBuilder ()
graphNode parent n heapAssoc =  -- ?? shall we avoid to cyclic draw Node, we cound add count parameter, when count > 100 , we abandoned the draw.
    (case n of
        NAp addr1 addr2 -> do
            appN <- addNode "app" VApp
            graphNode appN (aLookup heapAssoc addr1 (error "graphStackItems：graphNode")) heapAssoc
            graphNode appN (aLookup heapAssoc addr2 (error "graphStackItems：graphNode")) heapAssoc
            return appN
        NSupercomb funName argNameList expr -> do
            funDef <- addNode (funName ++ " " ++ (joinBy " ," argNameList) ) VLAMBDA
            b <- addNode (show expr) VValue 
            addEdge funDef b
            return funDef  -- the only reason to return funDef, is to pass the appN to the follow if else clausure.
        
        NNum i -> do
            addNode (show i)  VValue

        NInd addr -> do
            addNode ("NInd " ++ show addr) VValue

        NPrim str primitive -> do
            addNode ("primitive " ++ str) VValue
        
        NData tag argList -> do
            addNode ("NData " ++ show tag ++ show argList) VValue


    ) >>= (do
        if parent == "" 
          then \node -> return () 
          else \node -> addEdge parent node) --addEdge parent



-- graphExpr:: String -> Expr -> GraphBuilder ()
-- graphExpr parent t  = (
--     -- addNode "default" VVar
--   case t of 
--     EVar s -> addNode s        VVar
--     ENum i -> addNode (show i) VVar 

--     EAp rator rand -> do
--         appN <- addNode "app" VApp
--         graphExpr  appN  rator
--         graphExpr  appN  rand
--         return appN             -- the only reason to return funDef, is to pass the appN to the follow if else clausure.

--   ) >>= (do
--             if parent == "" 
--               then \node -> return () 
--               else \node -> addEdge parent node) --addEdge parent


graphAStack :: String ->Stack -> GraphBuilder ()
graphAStack  stackName  stack = do
    parent <- addNode stackName VVar
    graphStackItems parent  stack



graphAStackWithId :: (String, Stack ) -> ([V], [E])
graphAStackWithId  (id, stack) =    -- the id is the moduleName + n(counter) 
    evalGraph (graphAStack id stack) $ empty id  -- the key thing is here,  the state contains the cluster name(module name),   we will append the cluster name to the vertex string in graphviz structure



graphAllStackHelper :: [(String, Stack )] -> ([V], [E])
graphAllStackHelper stackids = 
  case stackids of
    []            ->  ([], [])
    (head : tail) ->  let (vs, es)   = graphAStackWithId head
                          (vst, est) = graphAllStackHelper tail
                      in  (vs ++ vst, es ++ est)


-- translate each term to tuple , fst of tuple is it's moduleName ++ index
stackToClusterId :: String -> [Stack] -> [(String, Stack)]
stackToClusterId moduleName stack =
  zipWith (\a b -> (moduleName ++ show b,a)) stack [1..]  -- the first item is the id which is the moduleName ++ n(counter) 


graphAllStack :: String -> [Stack] -> ([V], [E])
graphAllStack moduleName stack = graphAllStackHelper $ stackToClusterId moduleName stack




-- graphAll :: String -> [Stack] -> G.DotGraph String 
-- graphAll moduleName terms = 
--   let (vs, es) = graphAllStack moduleName terms;
--   in  G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String 


graphAllEx :: [String] -> [[Stack]] -> G.DotGraph String 
graphAllEx moduleName terms = 
  let ves {-- [(vs, es)] --} = zipWith graphAllStack moduleName terms;
      (vs, es) = foldl (\(v1, e1) (v2, e2) -> (v1++v2, e1++e2))  ([], []) ves
  in  G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String 
  


-- drawAst :: String -> String -> [Stack] -> IO ()
-- drawAst moduleName path terms = 
--   return path  >>=
--     G.addExtension (G.runGraphviz $ graphAll moduleName terms) G.Png
--     >>  putStrLn ("the image generated in :" ++ path)  
    


drawStackEx :: [String] -> String -> [[Stack]]-> IO ()
drawStackEx moduleNames path termss = 
    if length moduleNames /= length termss
        then 
            error "moduleNames /= length termss"
        else 
            return path  >>=
                G.addExtension (G.runGraphviz $ graphAllEx moduleNames termss) G.Png
                >>  putStrLn ("the image generated in :" ++ path)      
 