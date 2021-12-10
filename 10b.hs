import AOC

main = interact $ g . map (f [])

matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'

isOpen = flip elem "([{<"

getval '(' = 1
getval '[' = 2
getval '{' = 3
getval '<' = 4

f ys     []     = foldl' ((+) . (*5)) 0 $ map getval ys
f []     (x:xs) = if isOpen x then f [x] xs      else 0
f (y:ys) (x:xs) = if isOpen x then f (x:y:ys) xs else if matching y == x then f ys xs else 0

g = median . sort . filter (/=0)
  where
    median xs = let n = length xs in head $ drop (n `quot` 2) xs
