import AOC hiding(Down)

main = interact $ f . parselist p

data Direction = Up | Down | Forward deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (Direction, Int)
p = do
  d <- enump
  char ' '
  n <- integer
  return (d, n)

move (x, y, a) (Forward, n) = (x + n, y + a * n, a)
move (x, y, a) (Down, n)    = (x, y, a + n)
move (x, y, a) (Up, n)      = (x, y, a - n)

f xs = x * y
  where
    (x, y, a) = foldl' move (0, 0, 0) xs
