import AOC

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

    h = sortOn snd $ histo r
    r = applyN 10 step s
    step xs@(x:_) = x : concatMap g (getpairs xs)
    g p@(x, y) = [fromMaybe ' ' $ lookup p ts', y]

    getpairs [] = []
    getpairs [x] = []
    getpairs (x:y:xs) = (x, y):getpairs(y:xs)
