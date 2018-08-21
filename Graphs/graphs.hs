
import Data.Array

type Vertex = Char
type Table a = Array Vertex a
type Graph   = Table [Vertex]
type Edge    = (Vertex, Vertex)
type Bounds  = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [ (v,w) | v <- vertices g, w <- g!v ]

-- applies function argument to every table index/ entry pair
--  builds a new table
mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [ (v, f v (t ! v)) | v <- indices t]

outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g
      where numEdges v ws = length ws

indegree :: Graph -> Table Int
indegree g = outdegree (transposeG g)


buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

reverseE :: Graph -> [Edge]
reverseE g = [ (w, v) | (v, w) <- edges g]

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

graph = buildG ('a','j') [('a','b'), ('a','f'), ('b','c'),
                          ('b','e'), ('c','a'), ('c','d'),
                          ('e','d'), ('g','h'), ('g','j'),
                          ('h','f'), ('h','i'), ('h','j')]
                                     
                            
                                      
                                      
