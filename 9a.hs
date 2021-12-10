import AOC

main = interact $ f . map2 (read @Int . (:[]))

f = sum . map (+1) . catMaybes . concat . vtol2 . mapnbsv nbs4 g . ltov2
  where
    g x xs = if all (>x) xs then Just x else Nothing
