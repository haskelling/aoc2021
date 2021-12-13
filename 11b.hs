import AOC

main = interact $ f . map (map ((read :: String -> Int) . (:[])))

f xss = step (0, ltov2 xss)

step (n, xss) = if c then n' else step (n', mapnbsv nbs8 i $ ys)
  where
    n'              = succ n
    ys              = converge (mapnbsv nbs8 h) $ mapnbsv nbs8 g xss
    -- Left False means we've flashed
    -- Left True  means we've flashed and it's been processed
    c               = all (==Left True) $ concat $ vtol2 ys
    flash x         = if x > 9 then Left False else Right x
    g x xs          = flash $ succ x
    h (Right x) xs  = flash $ x + count (Left False) xs
    h (Left _) xs   = Left True
    i (Right x)   _ = x
    i (Left True) _ = 0
