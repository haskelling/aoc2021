import AOC

main = interact $ f . parselist p

p = do
  char 'o'
  b <- (char 'n' >> return True) <|> (string "ff" >> return False)
  string " x="
  x1 <- integer'
  string ".."
  x2 <- integer'
  string ",y="
  y1 <- integer'
  string ".."
  y2 <- integer'
  string ",z="
  z1 <- integer'
  string ".."
  z2 <- integer'
  return (b, (x1, y1, z1), (x2, y2, z2))

f xs = sum $ map getSize $ f' [] $ filter (\(_, (x1, y1, z1), (x2, y2, z2)) -> all inRange [x1, y1, z1, x2, y2, z2]) xs
  where
    inRange x = x >= -50 && x <= 50
    getSize (_, (x1, y1, z1), (x2, y2, z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)

    f' ss [] = ss
    f' ss (q:qs) = f' ss' qs
      where
        ss' = filter t1 $ q:concatMap (splitup q) ss
        fincr = filter incr
        incr = uncurry (<=)

        splitup (b', (x1', y1', z1'), (x2', y2', z2')) (b, (x1, y1, z1), (x2, y2, z2)) = do
          (u1, u2) <- fincr [(x1, min x2 (x1'-1)), (max x1 x1', min x2 x2'), (max x1 (x2'+1), x2)]
          (v1, v2) <- fincr [(y1, min y2 (y1'-1)), (max y1 y1', min y2 y2'), (max y1 (y2'+1), y2)]
          (w1, w2) <- fincr [(z1, min z2 (z1'-1)), (max z1 z1', min z2 z2'), (max z1 (z2'+1), z2)]
          guard $ not $ u1 == max x1 x1' && u2 == min x2 x2'
                     && v1 == max y1 y1' && v2 == min y2 y2'
                     && w1 == max z1 z1' && w2 == min z2 z2'
          return $ (b, (u1, v1, w1), (u2, v2, w2))

--
--
--  ####
--  ##....
--  ##....
--    ....
--
--  x1' .... x2'
--  x1 #|## x2
--
--  x1' .... x2'
--     x1 #|## x2
--
--        x1'.... x2'
--  x1 #### x2
--
--  x1'.... x2'
--        x1 #### x2
--
--       x1'....... x2'
--        x1 #### x2
--
--           x1'... x2'
--        x1 ###|#|## x2
