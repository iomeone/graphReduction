module Graph where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import Types
-- import qualified Data.Text.Lazy as TL

-- import Language


-- type CoreDef =  (String, Term)

-- type CoreAlter = (Int, [String], Term)

-- type CoreExpr = Term

data VLabeltype =  VLAMBDA | VApp | VVar | VValue
data  VLabel =VLabel String VLabeltype

type V = (String, VLabel)

data ELabel = ELHardlink
            | ELSymlink

type E = (String, String, ELabel)

type AstGraph = ([V], [E])

type Index = Int
type ClusterName = String
type NodeState = (ClusterName, Index)

type GraphBuilderState = (NodeState, AstGraph)
newtype GraphBuilder a = GB (GraphBuilderState -> (a, GraphBuilderState))

instance Functor GraphBuilder where
    -- fmap :: (a -> b) -> GraphBuilder a -> GraphBuilder b
    fmap f b = GB (\s -> let (v, s') = build b s in (f v, s'))

instance Applicative GraphBuilder where
    -- pure :: a -> GraphBuilder a
    pure x = GB (\s -> (x, s))
    -- (<*>) :: GraphBuilder (a -> b) -> GraphBuilder a -> GraphBuilder b
    bf <*> bx = GB (\s -> let (f, s') = build bf s in build (f <$> bx) s')

instance Monad GraphBuilder where
    -- (>>=) :: GraphBuilder a -> (a -> GraphBuilder b) -> GraphBuilder b
    b >>= f = GB (\s -> let (v, s') = build b s in build (f v) s')

getGraphBuilderState :: GraphBuilder GraphBuilderState
getGraphBuilderState = GB(\s -> (s, s))


empty :: String -> GraphBuilderState
empty clstName = ((clstName, 0), ([], []))

build :: GraphBuilder a -> GraphBuilderState -> (a, GraphBuilderState)
build (GB f) s = f s

evalGraph :: GraphBuilder a -> GraphBuilderState -> AstGraph
evalGraph b s = let (_, (_, g)) = build b s in g

-- let des = (getDes t)
-- in  do  
--     (nodeState, astGraph) <- getGraphBuilderState
--     addNode des -- $ getId nodeState des


addNode_ :: String -> VLabeltype -> GraphBuilder String
addNode_ desc vlabel = GB (\s ->
    let ((clsName, n), (vs, es)) = s
        nodeId = clsName ++ (show n) ++ ": " ++ desc
        vs' = vs ++ [(nodeId, VLabel desc vlabel)]
        s' = ((clsName, n+1), (vs', es))
    in (nodeId, s'))

addNode :: String -> VLabeltype -> GraphBuilder String
addNode desc lblType  = addNode_ desc lblType

addEdge_ :: String -> String -> ELabel -> GraphBuilder ()
addEdge_ from to elabel= GB (\s ->
    let (n, (vs, es)) = s
        es' = es ++ [(from, to, elabel)]
        s' = (n, (vs, es'))
    in ((), s'))

addEdge :: String -> String  -> GraphBuilder ()
addEdge from to = addEdge_ from to ELSymlink

-- addDef :: String -> CoreDef -> GraphBuilder ()
-- addDef parent (var, body) = do
--     n <- addNode $ "Def " ++ var
--     addEdge parent $ fst n
--     addExpr (fst n) body

-- addAlter :: String -> CoreAlter -> GraphBuilder ()
-- addAlter parent (tag, params, body) = do
--     n <- addNode $ "Alter <" ++ show tag ++ "> " ++ unwords params
--     addEdge parent (fst n)
--     addExpr (fst n) body

-- addExpr :: String -> CoreExpr -> GraphBuilder ()
-- addExpr parent expr = (
--     case expr of
--       TmAbs str term -> addNode $ "EVar " 
--         -- EVar x          -> addNode $ "EVar " ++ x
--         -- e@(ENum _)      -> addNode $ show e
--         -- e@(EConstr _ _) -> addNode $ show e
--         -- EAp lhs rhs     -> do n <- addNode "EAp"
--         --                       addExpr n lhs
--         --                       addExpr n rhs
--         --                       return n
--         -- ELet r ds e     -> do n <- addNode $ "ELet " ++ show r
--         --                       mapM (addDef n) ds
--         --                       addExpr n e
--         --                       return n
--         -- ECase e as      -> do n <- addNode "ECase"
--         --                       addExpr n e
--         --                       mapM (addAlter n) as
--         --                       return n
--         -- ELam ps e       -> do n <- addNode $ "ELam " ++ unwords ps
--         --                       addExpr n e
--         --                       return n
--     ) >>= addEdge parent


-- type ScDefn = (String, [String], Term)
-- type CoreScDefn = ScDefn

-- addScDefn :: String -> CoreScDefn -> GraphBuilder ()
-- addScDefn parent (var, params, body) = do
--     n <- addNode $ "ScDefn " ++ var ++ " " ++ unwords params
--     addEdge parent n
--     addExpr n body

-- addProgram :: CoreProgram -> GraphBuilder ()
-- addProgram scs = do
--     n <- addNode "Program"
--     mapM (addScDefn n) scs
--     return ()

astGraphParams :: G.GraphvizParams String VLabel ELabel () VLabel
astGraphParams = G.defaultParams




-- GraphVisParams vertexType vertexLabeltype edgeLabelType clusterType clusterLabelType
fileGraphParams :: G.GraphvizParams String VLabel ELabel () VLabel
fileGraphParams = G.defaultParams {
  G.globalAttributes =
    [ G.GraphAttrs
      [ G.Overlap (G.PrismOverlap Nothing)
      , G.OutputOrder G.EdgesFirst
      , G.RankDir G.FromTop
      , G.BgColor [G.toWColor G.Transparent]

      ]
    , G.NodeAttrs
       [ G.Style
         [ G.SItem G.Filled []
         ]
        -- ,G.Shape G.Circle
       ]
    ],
  G.fmtNode = \(v, VLabel description vl) -> case vl of
      VVar     -> ( G.toLabel description) : (G.Shape G.Circle) : ( colorAttribute $ G.RGB 0 255 255)
      VLAMBDA  -> ( G.toLabel description) :  (colorAttribute $ G.RGB 200 0 0)
      VApp     -> ( G.toLabel description) : (colorAttribute $ G.RGB 80 80 200)
      VValue     -> ( G.toLabel description) : (colorAttribute $ G.RGB 80 80 200),
  G.fmtEdge = \(from, to, el) -> case el of
      ELHardlink -> colorAttribute $ G.RGB 0 0 200
      ELSymlink  -> colorAttribute $ G.RGB 40 255 40
      } 
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]





-- mkDotAstGraph :: CoreProgram -> String
-- mkDotAstGraph prog = let (vs, es) = evalGraph (addProgram prog) empty
--                          dotGraph = G.graphElemsToDot astGraphParams vs es :: G.DotGraph String
--                          dotText = G.printDotGraph dotGraph :: TL.Text
--                      in TL.unpack dotText  

tgb :: GraphBuilder ()
tgb = do
  h <- addNode_ "hello" VApp
  w <- addNode_ "world" VLAMBDA
  addEdge_ h w ELHardlink



-- main :: IO ()
-- main = 
--   let 
--     (vs, es) = evalGraph tgb empty 
--     dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph String
--   in  do
--     G.addExtension (G.runGraphviz dotGraph) G.Png "graph"
--     putStrLn "hello world"

