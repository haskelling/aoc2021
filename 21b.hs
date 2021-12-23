import AOC
import Data.MemoTrie

main = interact $ f . map (read @Int . last . words)

f [n1, n2] = uncurry max $ f'' ((n1, 0), (n2, 0), True)
  where
    h = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

    f'' = memo f'
    f' ((n1, s1), (n2, s2), t) = if s1 >= 21 then (1, 0) else if s2 >= 21 then (0, 1) else y
      where
        y = sum $ do
                    (d, fac) <- h
                    let r = if t then let n' = up n1 d in f'' ((n', s1 + n'), (n2, s2), t')
                                 else let n' = up n2 d in f'' ((n1, s1), (n', s2 + n'), t')
                    return $ fac *$ r
        t' = not t
        up n d = (n + d - 1) `rem` 10 + 1
