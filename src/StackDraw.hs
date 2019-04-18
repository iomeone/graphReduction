module StackDraw (drawStackEx) where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import Common
import Graph
import State
import Types


type Stack = ([Addr], [(Addr, Node)])

data ParentString = NodeStr String | NameStr String | NoName



graphStackItems :: ParentString ->Stack -> Maybe VLabeltype -> Maybe ELabel -> GraphBuilder ()
graphStackItems parentStr (stack@(addr : addrs), heapAssoc) lblType edgeType = do
    
    

    (case parentStr of 
        NameStr s -> do
            addrStr <- addNode (show addr) (maybe VVar id lblType)
            nodeStr <- graphNode addrStr (aLookup heapAssoc addr (error "graphStackItems：graphNode")) heapAssoc addrs ELSymlink
            addEdge_ s addrStr (maybe ELSymlink id edgeType)
            return nodeStr
            
            
        NoName    -> do
            addrStr <- addNode (show addr) (maybe VVar id lblType)
            nodeStr <- graphNode addrStr (aLookup heapAssoc addr (error "graphStackItems：graphNode")) heapAssoc addrs ELSymlink
            return nodeStr

        NodeStr s -> do
            nodeStr <- graphNode s (aLookup heapAssoc addr (error "graphStackItems：graphNode")) heapAssoc addrs EStackLink
            -- this nodeStr is not the root of one node,  it is the node which is the same addr as next stack Node addr.
            -- addEdge_ s nodeStr (maybe ELSymlink id edgeType)
            return nodeStr
        ) 
            
            
    >>= \ x -> graphStackItems (NodeStr x) (addrs, heapAssoc) (Just VStack)  (Just EStackLink)-- next stack item draw 

           
   


graphStackItems parent (stack @ [], heapAssoc) lblType edgeType= do
    return ()

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
graphNode :: String -> Node -> [(Addr, Node)] -> [Addr] -> ELabel -> GraphBuilder String
graphNode parent n heapAssoc restStackAddr edgeType=  -- ?? shall we avoid to cyclic draw Node, we cound add count parameter, when count > 100 , we abandoned the draw.
    case n of
        NAp addr1 addr2 -> do
            appN <- addNode "app" VApp

                          
            a1 <- addNode 
                        -- (maybe (show addr1)  
                                -- -- if addr1 is not ValueNode, we just show addr1.
                                -- (\numNode -> show addr1 ++ " " ++ (show numNode)) $ aLookUpValueNode heapAssoc addr1 (error "addr1")) 
                                -- -- if addr1 is  ValueNode, we  show addr1 ++ (show Node).
                         (let node1 = aLookup heapAssoc addr1 (error "addr1.")
                          in (if isValueNodeSimple node1 || isPrimNodeSimple  node1
                                then show addr1 ++ " " ++ (show node1)
                                    else if isLambdaNodeSimple node1
                                        then show addr1 ++ "    λ : " ++ (showLambdaNodeSimple node1)
                                        else if isAppNodeSimple node1
                                            then show addr1 ++ "    (app)"
                                            else show addr1))


                        (if isPrimNodeSimple (aLookup heapAssoc addr1 (error "addr1.")) 
                            then VPrim
                            else VVar)

            a2 <- addNode (let node2 = aLookup heapAssoc addr2 (error "addr2.")
                           in
                            (if isValueNodeSimple  node2 || isPrimNodeSimple node2
                                then show addr2 ++ " " ++ (show node2)
                                    else if isLambdaNodeSimple node2
                                        then show addr2 ++ "    λ : " ++ (showLambdaNodeSimple node2)
                                        else if isAppNodeSimple node2
                                            then show addr2 ++ "    (app)"  
                                            else show addr2))




                           (if isValueNodeSimple (aLookup heapAssoc addr2 (error "addr2.")) 
                               then VNum 
                               else VVar )

            addEdge appN a1
            addEdge appN a2

            

            -- graphNode a1 (aLookup heapAssoc addr1 (error "graphStackItems：graphNode")) heapAssoc
            -- graphNode a2 (aLookup heapAssoc addr2 (error "graphStackItems：graphNode")) heapAssoc
            addEdge_ parent appN edgeType
            if restStackAddr == []
                then return appN
                else if head restStackAddr == addr1
                        then   return a1    
                        else if head restStackAddr == addr2
                            then return a2
                            else return appN
            
           
                            
        NSupercomb funName argNameList expr -> do
            funDef <- addNode (funName ++ " " ++ (joinBy " ," argNameList) ) VLAMBDA
            b <- addNode (show expr) VBody 
            addEdge funDef b

            addEdge_ parent funDef edgeType
            return funDef
            -- return funDef  -- the only reason to return funDef, is to pass the appN to the follow if else clausure.
        
        NNum i -> do
            n <- addNode (show i)  VNum
            addEdge_ parent n edgeType
            return n

        NInd addr -> do
            n <- addNode ("NInd " ++ show addr) VVar
            addEdge_ parent n edgeType
            return n

        NPrim str primitive -> do
            n <- addNode ("primitive " ++ str) VVar
            addEdge_ parent n edgeType
            return n
        
        NData tag argList -> do
            n <- addNode ("NData " ++ show tag ++ show argList) VNum
            addEdge_ parent n edgeType
            return n


    
    -- >>= (
    --     if parent == "" 
    --       then \node -> return node
    --       else \node -> do
    --                     addEdge parent node
    --                     return node) --addEdge parent



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
graphAStack  stackName  stack@(stackAddrs@(addrs), heapAssoc) = do
    parent <- addNode (stackName ++  (take ((length addrs) * 35) $ repeat  ' '))  VStackName
    graphStackItems (NameStr parent)  stack Nothing (Just ENameLink)



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
 