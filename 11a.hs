import AOC

main = interact $ f . map (map ((read :: String -> Int) . (:[])))

f xss = fst $ applyN 100 step (0, ltov2 xss)

step (n, xss) = (n + c, mapnbsv nbs8 i $ ys)
  where
    ys              = converge (mapnbsv nbs8 h) $ mapnbsv nbs8 g xss
    c               = count (Left True) $ concat $ vtol2 ys
    -- Left False means we've flashed
    -- Left True  means we've flashed and it's been processed
    flash x         = if x > 9 then Left False else Right x
    g x xs          = flash $ succ x
    h (Right x) xs  = flash $ x + count (Left False) xs
    h (Left _) xs   = Left True
    i (Right x)   _ = x
    i (Left True) _ = 0
