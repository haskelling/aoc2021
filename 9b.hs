import AOC
import qualified Data.Vector as V

main = interact $ f . map2 (read @Int . (:[]))

f = product . take 3 . sortBy (comparing Down) . map (succ . length . group . sort) . concat . vtol2 . mapnbsr nbs4 g . ltov2
  where
    g :: Int -> [((Int, Int), Int, [(Int, Int)])] -> [(Int, Int)]
    g x xs = concatMap (\(v, _, vs) -> v:vs) $ filter (\(_, x', _) -> x < x' && x' < 9) xs

mapnbsr :: [(Int, Int)]                     -- ^ The list of coordinate offsets
        -> (a -> [((Int, Int), a, b)] -> b) -- ^ The mapping function taking this node's value and [(coord, value, result)] of the neighbours
        -> Vector (Vector a)                -- ^ The original grid
        -> Vector (Vector b)                -- ^ The updated grid
mapnbsr nbs f m = r
  where
    r = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
    modify i x = f i $ mapMaybe (get x) nbs
    get' v (x, y) = do
      row <- v V.!? y
      row V.!? x
    get z0 z = do
      let z' = z0 + z
      vm <- get' m z'
      vr <- get' r z'
      return (z', vm, vr)
