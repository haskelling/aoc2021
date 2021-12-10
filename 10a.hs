import AOC

main = interact $ sum . map (f [])

matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'

isOpen = flip elem "([{<"

getval ')' = 3
getval ']' = 57
getval '}' = 1197
getval '>' = 25137

f _      []     = 0
f []     (x:xs) = if isOpen x then f [x] xs      else getval x
f (y:ys) (x:xs) = if isOpen x then f (x:y:ys) xs else if matching y == x then f ys xs else getval x
