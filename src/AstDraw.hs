module AstDraw where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import Types
import Graph

-- data Expr = TmVar { index :: Int, contextLength :: Int }
--           | TmAbs String Expr
--           | TmApp Expr Expr
--           | TmBind String -- not really a term, just a binding a free variable
--             deriving (Show, Eq)


-- getId:: NodeState -> String -> String
-- getId nodeState des = fst nodeState ++ "----" ++ des
build :: GraphBuilder a -> GraphBuilderState -> (a, GraphBuilderState)
build (GB f) s = f s


joinBy sep list = drop (length sep) $ concat $ map (\item -> sep ++ item) list

graphTerm:: String -> ScDefn -> GraphBuilder ()
graphTerm parent t  = 
    (case t of
        (funName, argNameList, expr) -> do
            funDef <- addNode (funName ++ " " ++ (joinBy " ," argNameList) ) VLAMBDA
            graphExpr funDef expr
            return funDef  -- the only reason to return funDef, is to pass the funDef to the follow if else clausure.
    ) >>= (do
        if parent == "" 
          then \node -> return () 
          else \node -> addEdge parent node) 
           

graphExpr:: String -> Expr -> GraphBuilder ()
graphExpr parent t  = (
    -- addNode "default" VVar
  case t of 
    EVar s -> addNode s        VVar
    ENum i -> addNode (show i) VVar 

    EAp rator rand -> do
        appN <- addNode "app" VApp
        graphExpr  appN  rator
        graphExpr  appN  rand
        return appN             -- the only reason to return funDef, is to pass the appN to the follow if else clausure.

    -- TmVar index len name ->  let des = (getDes t)
    --                          in  do  
    --                              (nodeState, astGraph) <- getGraphBuilderState
    --                              addNode des VVar -- $ getId nodeState des
    -- TmAbs str term       ->  let des = (getDes t)
    --                          in  do  
    --                              (nodeState, astGraph) <- getGraphBuilderState
    --                              abs <- addNode des VLAMBDA -- $ getId nodeState des
    --                              graphTerm abs  term
    --                              return abs

    -- TmApp rator rand     ->  do  (nodeState, astGraph) <- getGraphBuilderState
    --                              appN <- addNode "app" VApp -- $ getId nodeState "app"
    --                              graphTerm  appN  rator
    --                              graphTerm  appN  rand
    --                              return appN

  ) >>= (do
            if parent == "" 
              then \node -> return () 
              else \node -> addEdge parent node) --addEdge parent

--   where
--     getDes term = 
--         "default"
        -- case term of 
            -- TmVar index len name ->  "  " ++ name ++ "=E[" ++ show index ++"]";
            -- TmAbs str term  ->  "λ " ++ str;


            -- TmApp app arg ->  return ();
            -- TmBind str ->  return ();   
            
-- translate each term to tuple , fst of tuple is it's index
termToClusterId :: String -> [ScDefn] -> [(String, ScDefn)]
termToClusterId moduleName terms =
  zipWith (\a b -> (moduleName ++ show b,a)) terms [1..]


labelTerms :: String -> [ScDefn] ->  [(String, ScDefn)]
labelTerms moduleName terms  = 
  termToClusterId moduleName terms


graphATerm:: String -> ScDefn -> GraphBuilder ()
graphATerm header t = do
  parent <- addNode header VVar
  graphTerm parent $  t;

graphATermWithId::  (String, ScDefn) -> ([V], [E])
graphATermWithId  (id, t) = 
  evalGraph (graphATerm id t) $ empty id
  
-- graphAllTerm :: [Expr] -> [G.DotGraph String]

graphAllTermHelper termids = 
  case termids of
    []            ->  ([], [])
    (head : tail) ->  let (vs, es) = graphATermWithId head
                          (vst, est) = graphAllTermHelper tail
                      in  (vs ++ vst, es ++ est)



graphAllTerm :: String -> [ScDefn] -> ([V], [E])
graphAllTerm moduleName terms = graphAllTermHelper $ labelTerms moduleName terms
  


-- vsvsidTovs :: [V] ->[(String, VLabel)]
-- vsvsidTovs v = 
--   fmap   (\((vid, v), vlbl) -> (vid, vlbl))   v


graphAll :: String -> [ScDefn] -> G.DotGraph String 
graphAll moduleName terms = 
  let (vs, es) = graphAllTerm moduleName terms;
  in  G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String 

graphAllEx :: [String] -> [[ScDefn]] -> G.DotGraph String 
graphAllEx moduleName terms = 
  let ves {-- [(vs, es)] --} = zipWith graphAllTerm moduleName terms;
      (vs, es) = foldl (\(v1, e1) (v2, e2) -> (v1++v2, e1++e2))  ([], []) ves
  in  G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String 
  

-- graphFirstTerm :: [Expr] -> GraphBuilder ()
-- graphFirstTerm terms = 
--   graphATerm $ head terms

 

-- evalToDotGraph::String -> GraphBuilder ()-> G.DotGraph String
-- evalToDotGraph moduleName gb = 
--   let (vs, es) = evalGraph gb $ empty moduleName
--   in G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String
  

drawAst :: String -> String -> [ScDefn] -> IO ()
drawAst moduleName path terms = 
  return path  >>=
    G.addExtension (G.runGraphviz $ graphAll moduleName terms) G.Png
    >>  putStrLn ("the image generated in :" ++ path)  
    


drawAstEx :: [String] -> String -> [Program]-> IO ()
drawAstEx moduleNames path termss = 
    if length moduleNames /= length termss
        then 
            error "moduleNames /= length termss"
        else 
            return path  >>=
                G.addExtension (G.runGraphviz $ graphAllEx moduleNames termss) G.Png
                >>  putStrLn ("the image generated in :" ++ path)      
 