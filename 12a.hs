import AOC

main = interact $ f . map ((tail <$>) . span (/='-'))

f cs = length $ f' ["start"]
  where
    ns = nub $ map fst cs
    cs1 n = map snd $ filter ((==n) . fst) cs
    cs2 n = map fst $ filter ((==n) . snd) cs
    cs' "end" = []
    cs' n = cs1 n ++ cs2 n

    f' ns@("end":_) = [ns]
    f' ns@(n:_) = concatMap (\n' -> if isLower (head n') && n' `elem` ns then [] else f' (n':ns)) $ cs' n
