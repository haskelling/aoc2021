import AOC

main = interact' $ f . map (read :: String -> Int) . splitOn ","

f xs = minimum' (minimum xs) (maximum xs) cost
  where
    minimum' low high f | high - low < 3 = minimum $ map f [low .. high]
    minimum' low high f = let x = (low + high) `quot` 2
                          in  if f (x + 1) > f x
                                then minimum' low x f
                                else minimum' x high f

    cost p = sum $ map (g . abs . (p-)) xs
    g n = n * (n + 1) `quot` 2
