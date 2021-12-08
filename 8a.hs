import AOC

main = interact $ length . concatMap f

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

f = filter (`elem` [2, 3, 4, 7]) . map length . drop 11 . words
