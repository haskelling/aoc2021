import AOC hiding(tr)
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S

main = interactg $ f . rights . map doparse

p1 = do
  string "--- scanner "
  n <- integer
  string " ---"
  return n

p2 = do
  x <- integer'
  char ','
  y <- integer'
  char ','
  z <- integer'
  return (x, y, z)

doparse (s:ss) = do
  n <- parse p1 s
  let xs = parselist p2 ss
  return (n, xs)

newtype Face = Face Int deriving (Show, Eq, Read, Ord, Bounded)
newtype Spin = Spin Int deriving (Show, Eq, Read, Ord, Bounded)
data Orient  = O Face Spin deriving (Show, Eq, Read, Ord, Bounded)

xFace 0 (x, y, z) = ( x, y, z)
xFace 1 (x, y, z) = (-x,-y, z)
xFace 2 (x, y, z) = ( y, z, x)
xFace 3 (x, y, z) = (-y,-z, x)
xFace 4 (x, y, z) = ( z, x, y)
xFace 5 (x, y, z) = (-z,-x, y)

rotX  0 (x, y, z) = ( x, y, z)
rotX  1 (x, y, z) = ( x, z,-y)
rotX  2 (x, y, z) = ( x,-y,-z)
rotX  3 (x, y, z) = ( x,-z, y)

o (O (Face xf) (Spin s)) = rotX s . xFace xf

allOrients = [O (Face xf) (Spin s) | xf <- [0..5], s <- [0..3]]

f ts = S.size ps
  where
    tsm = M.fromList ts
    ns = map fst ts
    ps = g tsm [head ns] (tail ns) (M.singleton (head ns) id) (S.fromList $ snd $ head ts)

g _   []     _        _     ps = ps
g tsm (n:ns) unknowns trans ps = g tsm ns' unknowns' trans' ps'
  where
    ps'       = S.unions $ ps:pss
    ns'       = ns ++ ms
    unknowns' = unknowns \\ ms
    trans'    = M.unions $ trans:transs

    xs = tsm M.! n
    tr = trans M.! n

    (ms, transs, pss) = unzip3 $ concatMap (\m -> mapMaybe (comparelist m) allOrients) unknowns

    comparelist m ori = listToMaybe $ map (r . fst) $ filter ((>=12) . snd) h
      where
        r d = let tr' = tr . (+d) . o ori
              in  (m, M.singleton m tr', S.fromList $ map tr' ys)
        h  = histo [x - y | x <- xs, y <- map (o ori) ys]
        ys = tsm M.! m
