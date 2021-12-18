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

f [ds, fs] = length $ doFold (head fs') ds'
  where
    ds' = parselist p ds
    fs' = parselist p2 fs

    doFold f ps = nub $ map (doFold' f) ps
    doFold' ('x', n) (x, y) = if x < n then (x, y) else (n + n - x, y)
