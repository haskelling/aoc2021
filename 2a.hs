import AOC hiding(Down)

main = interact $ f . parselist p

data Direction = Up | Down | Forward deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (Direction, Int)
p = do
  d <- enump
  char ' '
  n <- integer
  return (d, n)

move (x, y) (Forward, n) = (x + n, y)
move (x, y) (Down, n)    = (x, y + n)
move (x, y) (Up, n)      = (x, y - n)

f xs = x * y
  where
    (x, y) = foldl' move (0, 0) xs
