import AOC

main = interact $ f . map read

f :: [Int] -> Int
f xs = count True $ zipWith (<) xs (tail xs)
