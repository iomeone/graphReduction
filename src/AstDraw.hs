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

graphTerm:: String -> Expr -> GraphBuilder ()
graphTerm parent t  = (
    addNode "default" VVar
--   case t of 
--     TmVar index len name ->  let des = (getDes t)
--                              in  do  
--                                  (nodeState, astGraph) <- getGraphBuilderState
--                                  addNode des VVar -- $ getId nodeState des
--     TmAbs str term       ->  let des = (getDes t)
--                              in  do  
--                                  (nodeState, astGraph) <- getGraphBuilderState
--                                  abs <- addNode des VLAMBDA -- $ getId nodeState des
--                                  graphTerm abs  term
--                                  return abs

--     TmApp rator rand     ->  do  (nodeState, astGraph) <- getGraphBuilderState
--                                  appN <- addNode "app" VApp -- $ getId nodeState "app"
--                                  graphTerm  appN  rator
--                                  graphTerm  appN  rand
--                                  return appN

  ) >>= (do
            if parent == "" 
              then \node -> return () 
              else \node -> addEdge parent node) --addEdge parent

  where
    getDes term = 
        "default"
        -- case term of 
            -- TmVar index len name ->  "  " ++ name ++ "=E[" ++ show index ++"]";
            -- TmAbs str term  ->  "λ " ++ str;


            -- TmApp app arg ->  return ();
            -- TmBind str ->  return ();   
            
-- translate each term to tuple , fst of tuple is it's index
termToClusterId :: String -> [Expr] -> [(String, Expr)]
termToClusterId moduleName terms =
  zipWith (\a b -> (moduleName ++ show b,a)) terms [1..]


labelTerms :: String -> [Expr] ->  [(String, Expr)]
labelTerms moduleName terms  = 
  termToClusterId moduleName terms


graphATerm:: String -> Expr -> GraphBuilder ()
graphATerm header t = do
  parent <- addNode header VVar
  graphTerm parent $  t;

graphATermWithId::  (String, Expr) -> ([V], [E])
graphATermWithId  (id, t) = 
  evalGraph (graphATerm id t) $ empty id
  
-- graphAllTerm :: [Expr] -> [G.DotGraph String]

graphAllTermHelper termids = 
  case termids of
    []            ->  ([], [])
    (head : tail) ->  let (vs, es) = graphATermWithId head
                          (vst, est) = graphAllTermHelper tail
                      in  (vs ++ vst, es ++ est)



graphAllTerm :: String -> [Expr] -> ([V], [E])
graphAllTerm moduleName terms = graphAllTermHelper $ labelTerms moduleName terms
  


-- vsvsidTovs :: [V] ->[(String, VLabel)]
-- vsvsidTovs v = 
--   fmap   (\((vid, v), vlbl) -> (vid, vlbl))   v


graphAll :: String -> [Expr] -> G.DotGraph String 
graphAll moduleName terms = 
  let (vs, es) = graphAllTerm moduleName terms;
  in  G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String 

graphAllEx :: [String] -> [[Expr]] -> G.DotGraph String 
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
  

drawAst :: String -> String -> [Expr] -> IO ()
drawAst moduleName path terms = 
  return path  >>=
    G.addExtension (G.runGraphviz $ graphAll moduleName terms) G.Png
    >>  putStrLn ("the image generated in :" ++ path)  
    


drawAstEx :: [String] -> String -> [[Expr]]-> IO ()
drawAstEx moduleNames path termss = 
    if length moduleNames /= length termss
        then 
            error "moduleNames /= length termss"
        else 
            return path  >>=
                G.addExtension (G.runGraphviz $ graphAllEx moduleNames termss) G.Png
                >>  putStrLn ("the image generated in :" ++ path)      
 