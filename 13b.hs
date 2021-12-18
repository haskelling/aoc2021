import AOC

main = interactg f

p :: Parser (Int, Int)
p = do
  x1 <- integer
  string ","
  x2 <- integer
  return (x1, x2)

p2 :: Parser (Char, Int)
p2 = do
  string "fold along "
  c <- anyChar
  char '='
  d <- integer
  return (c, d)

f [ds, fs] = Str $ ('\n':) $ unlines $ map2 (bool ' ' '#') $ getBools $ foldl' doFold ds' fs'
  where
    ds' = parselist p ds
    fs' = parselist p2 fs

    doFold ps f = nub $ map (doFold' f) ps
    doFold' ('x', n) (x, y) = if x < n then (x, y) else (n + n - x, y)
    doFold' ('y', n) (x, y) = if y < n then (x, y) else (x, n + n - y)

    getBools ps = [[ (x, y) `elem` ps | x <- [minimum xs .. maximum xs]] | y <- [minimum ys .. maximum ys]]
      where
        xs = map fst ps
        ys = map snd ps
