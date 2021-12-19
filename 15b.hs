import AOC hiding (between)
import Algorithm.Search
import qualified Data.Vector as V

main = interact $ f . map2 (read @Int . (:[]))

between x1 x2 x = x >= x1 && x <= x2

f grid0 = fromMaybe 0 $ fst <$> aStar nbs d (manhattan . (end-)) (==end) start
  where
    grid = enlarge $ map enlargex grid0
    enlargex = concat <$> mapM (map  . add') [0..4]
    enlarge  = concat <$> mapM (map2 . add') [0..4]
    add' x y = (x + y - 1) `rem` 9 + 1

    nbs v = filter (\(x, y) -> between (fst start) (fst end) x && between (snd start) (snd end) y) $ map (v+) nbs4
    start = (0, 0)
    end = (length (head grid) - 1, length grid - 1)
    d _ v = val v
    grid' = ltov2 grid
    val (x, y) = grid' V.! y V.! x
