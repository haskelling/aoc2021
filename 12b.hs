import AOC

main = interact $ f . map ((tail <$>) . span (/='-'))

f cs = length $ f' ["start"]
  where
    ns = nub $ map fst cs
    cs1 n = map snd $ filter ((==n) . fst) cs
    cs2 n = map fst $ filter ((==n) . snd) cs
    cs' "end" = []
    cs' n = filter (/="start") $ cs1 n ++ cs2 n

    f' ns@("end":_) = [ns]
    f' ns@(n:_) = concatMap (\n' -> if valid (n':ns) then f' (n':ns) else []) $ cs' n

    valid [] = True
    valid (n:ns) | isLower (head n) = if n `elem` ns then valid' ns else valid ns
    valid (n:ns)                    = valid ns
    valid' [] = True
    valid' (n:ns) | isLower (head n) = n `notElem` ns && valid' ns
    valid' (n:ns)                    = valid' ns
