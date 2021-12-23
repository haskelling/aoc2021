import AOC

main = interact' $ f . map (read :: String -> Int) . splitOn ","

f = sum . map snd . applyN 80 step' . histo
  where
    step (0, n) = [(6, n), (8, n)]
    step (t, n) = [(t-1, n)]
    step' = map (foldl1' (\(t, n) (_, m) -> (t, n + m))) . groupOn fst . concatMap step
