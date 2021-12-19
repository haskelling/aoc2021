import AOC

main = interact' f

integer' = do
  s <- (char '-' >> return (-1)) <|> return 1
  i <- integer
  return $ s * i

p = do
  string "target area: x="
  x1 <- integer'
  string ".."
  x2 <- integer'
  string ", y="
  y1 <- integer'
  string ".."
  y2 <- integer'
  return (x1, x2, y1, y2)

step (x, y, vx, vy, x1, x2, y1, y2, maxy) = if x' > x2 || (y' < y1 && vy < 0) || (vx < 1 && x < x1)
                                              then Nothing
                                              else if x' >= x1 && x' <= x2 && y' >= y1 && y' <= y2
                                                     then Just maxy'
                                                     else step (x', y', vx', vy', x1, x2, y1, y2, maxy')
  where
    (x', y', vx', vy') = (x + vx, y + vy, case compare 0 vx of LT -> vx - 1; EQ -> vx; GT -> vx + 1, vy - 1)
    maxy' = if y' > maxy then y' else maxy

f s = maximum z
  where
    z = mapMaybe step [(0, 0, vx, vy, x1, x2, y1, y2, minBound) | vx <- [1 .. x2 * 2], vy <- [1 .. x2 * 2]]
    Right (x1, x2, y1, y2) = parse p s
