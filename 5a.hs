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
    getps ((x1, y1), (x2, y2)) | x1 == x2 && y1 < y2 = [(x1, y) | y <- [y1..y2]]
    getps ((x1, y1), (x2, y2)) | x1 == x2            = [(x1, y) | y <- [y2..y1]]
    getps ((x1, y1), (x2, y2)) | y1 == y2 && x1 < x2 = [(x, y1) | x <- [x1..x2]]
    getps ((x1, y1), (x2, y2)) | y1 == y2            = [(x, y1) | x <- [x2..x1]]
    getps _ = []
