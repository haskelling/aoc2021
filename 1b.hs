import AOC

main = interact $ f . sw . map read

sw :: [Int] -> [Int]
sw xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail $ tail xs)

f :: [Int] -> Int
f xs = count True $ zipWith (<) xs (tail xs)
