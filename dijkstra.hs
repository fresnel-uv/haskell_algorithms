import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Vertex = Int
type Weight = Int
type Graph = M.Map Vertex [(Vertex, Weight)]

dijkstra :: Graph -> Vertex -> (M.Map Vertex Weight, M.Map Vertex (Maybe Vertex))
dijkstra graph source = go initialDist initialPrev (M.keys graph)
  where
    vertices = M.keys graph
    inf = maxBound `div` 2

    initialDist = M.fromList [(v, if v == source then 0 else inf) | v <- vertices]
    initialPrev = M.fromList [(v, Nothing) | v <- vertices]

    go dist prev [] = (dist, prev)
    go dist prev q =
      let u = minimumBy (comparing (dist M.!)) q
          du = dist M.! u
          neighbors = fromMaybe [] (M.lookup u graph)
          (dist', prev') = relaxAll u du neighbors dist prev
          q' = filter (/= u) q
      in go dist' prev' q'

    relaxAll u du [] dist prev = (dist, prev)
    relaxAll u du ((v, w):es) dist prev =
      let alt = du + w
          dv  = dist M.! v
      in if alt < dv
           then relaxAll u du es
                  (M.insert v alt dist)
                  (M.insert v (Just u) prev)
           else relaxAll u du es dist prev

-- Path reconstruction
shortestPath :: Vertex -> Vertex -> M.Map Vertex (Maybe Vertex) -> [Vertex]
shortestPath source target prev = reverse (build target)
  where
    build v =
      case M.lookup v prev of
        Nothing       -> [v | v == source]
        Just Nothing  -> [v | v == source]
        Just (Just u) -> v : build u
        
----------------------------------------
-- Example graph
----------------------------------------

exampleGraph :: Graph
exampleGraph = M.fromList
  [ (1, [(2, 7), (3, 9), (6, 14)])
  , (2, [(1, 7), (3, 10), (4, 15)])
  , (3, [(1, 9), (2, 10), (4, 11), (6, 2)])
  , (4, [(2, 15), (3, 11), (5, 6)])
  , (5, [(4, 6), (6, 9)])
  , (6, [(1, 14), (3, 2), (5, 9)])
  ]

main :: IO ()
main = do
  let (dist, prev) = dijkstra exampleGraph 1
  putStrLn "Distances:"
  print dist
  putStrLn "Predecessors:"
  print prev

  putStrLn "\nShortest path from 1 to 5:"
  print (shortestPath 1 5 prev)

  putStrLn "Shortest path from 1 to 4:"
  print (shortestPath 1 4 prev)
