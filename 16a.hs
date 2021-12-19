import AOC
import qualified Text.Parsec as P

main = interact' f

d '0' = [0,0,0,0]
d '1' = [0,0,0,1]
d '2' = [0,0,1,0]
d '3' = [0,0,1,1]
d '4' = [0,1,0,0]
d '5' = [0,1,0,1]
d '6' = [0,1,1,0]
d '7' = [0,1,1,1]
d '8' = [1,0,0,0]
d '9' = [1,0,0,1]
d 'A' = [1,0,1,0]
d 'B' = [1,0,1,1]
d 'C' = [1,1,0,0]
d 'D' = [1,1,0,1]
d 'E' = [1,1,1,0]
d 'F' = [1,1,1,1]
d _   = []

digi x = tokenPrim show (const . const) (\y -> if y==x then Just x else Nothing)

data I = Lit Int Integer | Op Int Int [I] deriving (Show, Eq, Read)

instance ReadBin Int where
  toBin 0 = Just 0
  toBin 1 = Just 1
  toBin _ = Nothing

lit :: Parsec [Int] () Integer
lit = lit' 0
  where
    lit' y = do
      t <- anyToken
      x <- toInteger <$> intn 4
      (if t == 1 then lit' else return) $ y * 16 + x

op :: Parsec [Int] () [I]
op = do
  i <- anyToken
  if i == 0 then do
    l <- intn 15
    fromRight [] . P.parse (many1 instr) "" <$> P.count l anyToken
  else do
    l <- intn 11
    P.count l instr

intn :: Int -> Parsec [Int] () Int
intn n = fromMaybe 0 . readBin <$> P.count n anyToken

instr :: Parsec [Int] () I
instr = do
  v <- intn 3
  t <- intn 3
  case t of
    4 -> Lit v <$> lit
    _ -> Op v t <$> op

versum (Lit v _) = v
versum (Op v _ is) = v + sum (map versum is)

f = versum . fromRight (Lit 0 0) . P.parse instr "" . concatMap d
