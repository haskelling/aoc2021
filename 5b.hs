import AOC

main = interact (f . parselist p)

p :: Parser ((Int, Int), (Int, Int))
p = do
  x1 <- integer
  char ','
  y1 <- integer
  string " -> "
  x2 <- integer
  char ','
  y2 <- integer
  return ((x1, y1), (x2, y2))

f = length . filter (>1) . map length . group . sort . concatMap getps
  where
    getps ((x1, y1), (x2, y2)) | x1 == x2 && y1 == y2 = [(x1, y1)]
    getps ((x1, y1), (x2, y2)) = (x1, y1) : getps ((next x1 x2, next y1 y2), (x2, y2))
      where
        next x1 x2 = case x1 `compare` x2 of
                       LT -> x1 + 1
                       EQ -> x1
                       GT -> x1 - 1
