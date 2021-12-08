import AOC

main = interactg f

f (x:xs) = last rns * (sum $ filter (`notElem` rns) (concat $ head rbs))
  where
    nums = map (read :: String -> Int) $ splitOn "," $ head x
    boards = bs ++ map transpose bs
    bs = map (map $ map (read :: String -> Int) . words) xs

    nums' = inits nums
    isWinner ns = any (all (`elem` ns))
    (rns, rbs) = head $ filter ((/=[]) . snd) $ map (\ns -> (ns, filter (isWinner ns) boards)) nums'
