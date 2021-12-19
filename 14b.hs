import AOC
import Data.MemoTrie

main = interactg f

p :: Parser ((Char, Char), Char)
p = do
  x1 <- anyChar
  x2 <- anyChar
  string " -> "
  x3 <- anyChar
  return ((x1, x2), x3)

f [[s], ts] = snd (last h) - snd (head h)
  where
    ts' = parselist p ts

--     h = sortOn snd $ histo r
--     step xs@(x:_) = x : concatMap g (getpairs xs)

    h = sortOn snd $ combine $ (head s, 1) : concatMap dm (zipWith (40,,) s (tail s))

    dm = memo d
    d :: (Int, Char, Char) -> [(Char, Int)]
    d (0, x, y) = [(y, 1)]
    d (n, x, y) = combine $ dm (n - 1, x, x') ++ dm (n - 1, x', y)
      where
        x' = fromMaybe ' ' $ lookup (x, y) ts'

    combine = map (\xs -> (fst $ head xs, sum $ map snd xs)) . groupOn fst . sort
