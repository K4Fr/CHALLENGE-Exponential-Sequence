one = 1 : repeat 0

x = 0 : one

diagonals f xs []         = []
diagonals f [] ys         = []
diagonals f (x:xs) (y:ys) = [f x y] : zipWith (:) (map (f x) ys) (diagonals f xs (y:ys))

u .+. v = zipWith (+) u v
u .*. v = map sum (diagonals (*) u v)
u .^ 0 = one
u .^ n | even n    = v .*. v
       | otherwise = u .*. (v .*. v)
  where m = n `div` 2
        v = u .^ m

-- NB: 1 : v represents 1 + x * v
soln = 1 : strictList ((soln .^ 3) .+. ((x .^ 4) .*. (soln .^ 13)))

strictList [] = []
strictList (x:xs) = x `seq` x : strictList xs

main = print (soln !! 2048)
