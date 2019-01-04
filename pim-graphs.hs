import Test.HUnit
import Data.List

main = runTestTT $ TestList [
    shouldColorTrivialGraph,
    shouldColorCircleWithEvenElements,
    shouldColorCircleWithMultipleEdgesPerVertex,
    shouldFailOnNonBipariteGraph
    ]

newtype Vertex = Vertex Int deriving (Eq, Show)
data Edge = Edge Vertex Vertex deriving (Eq, Show)
data Graph = Graph [Vertex] [Edge]
data Color = Red | Blue deriving (Eq, Show)

shouldColorTrivialGraph = let v1 = Vertex 1
                              v2 = Vertex 2
                              v3 = Vertex 3
                              edges = [Edge v1 v2, Edge v2 v3]
                          in TestCase $ assertEqual ""
                            (Just [(v1, Red), (v2, Blue), (v3, Red)])
                            (isTwoColorable (Graph [v1, v2, v3] edges))

shouldColorCircleWithEvenElements = let v1 = Vertex 1
                                        v2 = Vertex 2
                                        v3 = Vertex 3
                                        v4 = Vertex 4
                                        edges = [Edge v1 v2, Edge v2 v3, Edge v3 v4, Edge v4 v1] 
                                    in TestCase $ assertEqual ""  
                                    (Just [(v1, Red), (v2, Blue), (v3, Red), (v4, Blue)])
                                    (isTwoColorable (Graph [v1, v2, v3, v4] edges))
                                    
shouldColorCircleWithMultipleEdgesPerVertex = let   v1 = Vertex 1
                                                    v2 = Vertex 2
                                                    v3 = Vertex 3
                                                    v4 = Vertex 4
                                                    edges = [Edge v1 v2, Edge v1 v3, Edge v2 v4, Edge v3 v4] 
                                                in TestCase $ assertEqual ""  
                                                    (Just [(v1, Red), (v2, Blue), (v4, Red), (v3, Blue)])
                                                    (isTwoColorable (Graph [v1, v2, v3, v4] edges))                            

shouldFailOnNonBipariteGraph = let  v1 = Vertex 1
                                    v2 = Vertex 2
                                    v3 = Vertex 3
                                    edges = [Edge v1 v2, Edge v2 v3, Edge v1 v3]
                                in TestCase $ assertEqual ""
                                    Nothing
                                    (isTwoColorable (Graph [v1, v2, v3] edges))
                   

isTwoColorable :: Graph -> Maybe [(Vertex, Color)]
isTwoColorable (Graph [] _) = Nothing
isTwoColorable g@(Graph (v:_) es) = maybe Nothing (Just) (colorGraph (Just []) v Red es)

-- colorVertices :: [Edge] -> Vertex -> Color -> [(Vertex, Color)] -> [(Vertex, Color)]
-- colorVertices es v c r =    let resultVertices = map fst r
--                                 connections = filter (\(Edge v1 v2) -> (v1 == v || v2 == v)) es
--                                 neighbors = map (\(Edge v1 v2) -> if v1 == v then v2 else v1) connections
--                             in if v `elem` resultVertices 
--                                 then if all (\(_, rc) -> rc == c) (filter (\(vr, _) -> v == vr) r) then r else error "graph is not 2-colorable"
--                                 else foldl (\r' n -> colorVertices (es \\ connections) n (invertColor c) r') (r ++ [(v, c)]) neighbors

invertColor Red = Blue
invertColor Blue = Red

colorGraph :: Maybe [(Vertex, Color)] -> Vertex -> Color -> [Edge] -> Maybe [(Vertex, Color)]
colorGraph Nothing _ _ _ = Nothing
colorGraph result@(Just coloring) v color edges = 
    continue $ tryGet v coloring
    where
        continue (Just c)
            | c == color = result -- vertex already colored with desired color
            | otherwise = Nothing -- vertex already colored with a different color: invalid graph
        continue Nothing = let connections = filter (\(Edge v1 _) -> (v == v1)) edges
                               neighborVertices = map (\(Edge _ v2) -> v2) connections
                           in foldl (\ctx neighbor -> colorGraph ctx neighbor (invertColor color) edges) (Just (coloring ++ [(v, color)])) neighborVertices

tryGet :: Vertex -> [(Vertex, Color)] -> Maybe Color
tryGet _ [] = Nothing
tryGet v ((vr, cr):rs) = if vr == v 
                           then Just cr
                           else tryGet v rs