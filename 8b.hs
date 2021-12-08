import AOC

main = interact $ sum . map (f . map sort . words)

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
--
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
--
-- 2 must be cf (1)
-- 3 must be acf (7)
-- 4 must be bdcf (4)
-- 5 must be acdeg or acdfg or abdfg (adg)
-- 6 must be abcefg or abdefg or abcdfg (abfg)
-- 7 must be abcdefg (0)

f xs = read $ concatMap show $ map value os
  where
    getl n  = head $ filter ((==n) . length) xs'
    getl' n = common $ map head $ group $ sort $ filter ((==n) . length) xs'
    cf      = getl 2
    acf     = getl 3
    bdcf    = getl 4
    bd      = filter (`notElem` cf) bdcf
    adg     = getl' 5
    abfg    = getl' 6

    common [x]    = x
    common (x:xs) = filter (`elem` x) $ common xs

    a = head $ filter (`notElem` cf) acf
    b = head $ common [abfg, bd]
    c = head $ filter (/=f) cf
    d = head $ filter (/=b) bd
    e = head $ filter (`notElem` [a,b,c,d,f,g]) "abcdefg"
    f = head $ filter (`notElem` [a,b,g]) abfg
    g = head $ filter (/=a) $ common [abfg, adg]

    value xs | xs == sort [a,b,c,e,f,g]   = 0
    value xs | xs == sort [c,f]           = 1
    value xs | xs == sort [a,c,d,e,g]     = 2
    value xs | xs == sort [a,c,d,f,g]     = 3
    value xs | xs == sort [b,c,d,f]       = 4
    value xs | xs == sort [a,b,d,f,g]     = 5
    value xs | xs == sort [a,b,d,e,f,g]   = 6
    value xs | xs == sort [a,c,f]         = 7
    value xs | xs == sort [a,b,c,d,e,f,g] = 8
    value xs | xs == sort [a,b,c,d,f,g]   = 9

    xs' = take 10 xs ++ os
    os  = drop 11 xs
