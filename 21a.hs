import AOC

main = interact $ f . map (read @Int . last . words)

f [n1, n2] = f' die (n1, 0) (n2, 0) 0
  where
    die = cycle [1..100]

    f' (d1:d2:d3:d4:d5:d6:ds) (n1, s1) (n2, s2) t =
      if s1' >= 1000
        then s2 * (t * 6 + 3)
        else if s2' >= 1000
          then s1 * (t' * 6)
          else y
      where
        y = f' ds (n1', s1') (n2', s2') t'
        t' = succ t
        n1' = (n1 + d1 + d2 + d3 - 1) `rem` 10 + 1
        s1' = s1 + n1'
        n2' = (n2 + d4 + d5 + d6 - 1) `rem` 10 + 1
        s2' = s2 + n2'
