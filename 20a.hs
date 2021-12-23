import AOC
import qualified Data.Vector as V

main = interactg f

mapnbsv' :: [(Int, Int)]       -- ^ The list of coordinate offsets
        -> (a -> [a] -> b)    -- ^ The mapping function
        -> Vector (Vector a)  -- ^ The original grid
        -> Vector (Vector b)  -- ^ The updated grid
mapnbsv' nbs f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ map (fromMaybe i . get x) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

f [[alg],grid] = count True $ concat $ vtol2 $ step $ step $ ltov2 $ applyN 5 enlarge $ grid'
  where
    enlarge xss = emptyrow ++ map (\xs -> [False] ++ xs ++ [False]) xss ++ emptyrow
      where
        emptyrow = [replicate (2 + length (head xss)) False]
    step = mapnbsv' [(-1,-1), (0,-1), (1,-1), (-1,0), (0,0), (1,0), (-1,1), (0,1), (1,1)] g
    alg' = map (=='#') alg
    grid' = map2 (=='#') grid
    g _ xs = alg' !! (fromMaybe 0 $ readBin xs)
