import AOC

main = interact f

data SF = Val Int | Br SF SF deriving (Eq, Read, Show)

p :: Parser SF
p = (do
       char '['
       p1 <- p
       char ','
       p2 <- p
       char ']'
       return $ Br p1 p2
    ) <|> (Val <$> integer)

ttol :: SF -> [(Int, Int, Int)]
ttol sf = ttol' 0 0 sf
  where
    ttol' n m (Val v) = [(v, n, m)]
    ttol' n m (Br p1 p2) = (ttol' (n + 1) (m + 1) p1) ++ (ttol' 0 (m + 1) p2)

ltot :: [(Int, Int, Int)] -> SF
ltot xs = fst $ ltot' xs
  where
    ltot' ((x, 0, _):xs) = (Val x, xs)
    ltot' ((x, n, _):xs) = let (p1, xs')  = ltot' $ (x, n - 1, 0):xs
                               (p2, xs'') = ltot' $ xs'
                           in  (Br p1 p2, xs'')

sfadd p1 p2 = ltot $ reduce $ ttol $ Br p1 p2

reduce = converge (sfsplit . converge sfexplode)
sfsplit ((x, n, m):xs) | x >= 10 = let (y, r) = x `quotRem` 2 in (y, n + 1, m + 1):(y + r, 0, m + 1):xs
sfsplit (x:xs)                   = x:sfsplit xs
sfsplit []                       = []

sfexplode ((x, n, m):(a, u, 5):(b, 0, 5):(y, q, r):xs) = (x + a, n, m):(0, u - 1, 4):(b + y, q, r):xs
sfexplode ((x, n, m):(a, u, 5):(b, 0, 5):[])           = (x + a, n, m):(0, u - 1, 4):[]
sfexplode ((a, u, 5):(b, 0, 5):(y, q, r):xs)           = (0, u - 1, 4):(b + y, q, r):xs
sfexplode (x:xs)                                       = x:sfexplode xs
sfexplode []                                           = []

magnitude (Val x) = x
magnitude (Br p1 p2) = 3 * magnitude p1 + 2 * magnitude p2

f = magnitude . foldl1' sfadd . parselist p
