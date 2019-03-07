module Streda_CV03_fold where

-- pocet nul vo vektore/matici pomocou foldr/l
pocetNul :: [Int] -> Int
pocetNul xs = foldl (\a -> \b -> if b == 0 then a + 1 else a) 0 xs
pocetNul' xs = foldr (\b -> \a -> if b == 0 then a + 1 else a) 0 xs


--porovnaj :: (Int, Int) -> Int -> (Int, Int)
--porovnaj (max1, max2) b
--     | b < max1 = (b, max1)
--     | max1 < b && b < max2 = (max1, b)
--     | otherwise = (max1, max2)

druhyNajvacsi :: [Int] -> Int        
druhyNajvacsi [] = error "prazdny zoznam"
druhyNajvacsi [x] = 0
druhyNajvacsi (x:y:xs) = undefined --snd $ foldl porovnaj (min x y, max x y) xs


-- rozdiel max a minimalneho prvku vo vektore/matici
maxmin :: [Int] -> Int
maxmin xs = uncurry (-) $ foldl (\(vela, malo) x -> (max vela x, min malo x)) (-1000,1000) xs

pom :: [Int] -> (Int, Int)
pom xs = foldl (\(vela, malo) x -> (max vela x, min malo x)) ((-1000),1000) xs

maxmin' :: [[Int]] -> (Int, Int)
maxmin' xss = foldl (\(vela,malo) ->  \(velaxs, maloxs) -> 
                     (max vela velaxs, min malo maloxs)) 
                          ((-1000),1000) (map pom xss)


------------------------- definujte foldr pomocou foldr1
 
-- scanr/scanl

-- scanl f z [a1, ..., an] = [z, f z a1, f (f z a1) a2, ...]
-- scanl (\x -> \y -> 2*x + y) 4 [1,2,3] =  [4,9,20,43] 
-- foldl (\x -> \y -> 2*x + y) 4 [1,2,3] = 43

-- definujte myfoldl pomocou scanl
myfoldl f z xs = undefined


-- definujte myscanl pomocou foldl
myscanl f z xs = undefined


-- [x0, x1, ... xn-1] -> ([x1,x4,x7,...], [x2,x5,x8,...])
kazdyTretiDoKosa    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa    xs    = (z1, z2)
                                   where (z1, z2, i) = foldl (\(z1, z2, i) -> \x -> case i of 
                                                            0 -> (z1, z2, 1);
                                                            1 -> ((x:z1), z2, 2);
                                                            2 -> (z1, (x:z2), 0)) 
                                          ([], [], 1)
                                          xs
                                                                           

-- kazdyTretiDoKosa [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])

--kazdyTretiDoKosa'    :: [Int] -> ([Int], [Int])
--kazdyTretiDoKosa'    xs    = foldl (\(z1,z2) -> \(i,x) -> )
-- bohuzial, foldr pocita od konca...
-- kazdyTretiDoKosa' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])

kazdyTretiDoKosa''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa''    xs    = undefined  -- pozri v priprave

-- kazdyTretiDoKosa'' [2..20] = ([18,15,12,9,6,3],[19,16,13,10,7,4])
                                                                
kazdyTretiDoKosa'''    :: [Int] -> ([Int], [Int])
kazdyTretiDoKosa'''    xs    = undefined -- pozri v priprave
                                
-- kazdyTretiDoKosa''' [2..20] = ([4,7,10,13,16,19],[3,6,9,12,15,18])
