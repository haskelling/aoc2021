import AOC

main = interact $ f . (map (map (=='1')))

f xss = fromMaybe 0 $ readBin co2 * readBin o2
  where
    co2 = g (>=) [] xss
    o2  = g (<)  [] xss

    g c rs [xs] = rs ++ xs
    g c rs xss  = g c (rs ++ [r]) xss'
      where
        hs = map head xss
        r = count True hs `c` count False hs
        xss' = map tail $ filter ((==r) . head) xss
