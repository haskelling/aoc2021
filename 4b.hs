import AOC

main = interactg f

f (x:xs) = r nums' boards
  where
    nums = map (read :: String -> Int) $ splitOn "," $ head x
    bs = map (map $ map (read :: String -> Int) . words) xs
    boards = map (\b -> (b, transpose b)) bs

    nums' = inits nums
    isWinner ns (b1, b2) = any (all (`elem` ns)) (b1 ++ b2)
    calcScore (rns, (rb, _)) = last rns * (sum $ filter (`notElem` rns) $ concat rb)

    r (ns:nss) [b] = if isWinner ns b then calcScore (ns, b) else r nss [b]
    r (ns:nss) bs' = r nss $ filter (not . isWinner ns) bs'
