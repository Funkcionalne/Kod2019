module Priprava_CV02 where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort,nub)

-- definujte predikat pre usporiadany/rastuci/nerastuci zoznam
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:xs@(y:ys)) = x<=y && ordered xs

ordered' :: [Int] -> Bool
ordered' [] = True
ordered'  xs = all (\(x,y) -> x <= y) $ zip (init xs) (tail xs)


-- definujte a overte nejaku vlastnost funkcie ordered

qch1 = quickCheck( \xs -> ordered xs == ordered' xs )
qch1v = verboseCheck(\xs -> ordered $ sort xs  )
qch1v' = verboseCheck(\xs -> ordered' $ sort xs  )

qch1r = verboseCheck(\xs -> (length xs > 1) ==> not $ ordered $ reverse $ sort xs  )

-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x>y then y:(bubble(x:xs)) else x:(bubble(y:xs))

-- pouzite bubble na bubbleSort
bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
e = bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
--[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]

-- definujte a overte nejaku vlastnost funkcie bubbleSort
qch2 = quickCheck( \xs -> bubbleSort xs == sort xs)
qch2v = undefined -- verboseCheck( \xs -> cond ==> proposition )

powerSet :: [t] -> [[t]]
powerSet [] = [[]]
powerSet (x:xs) = let ps = powerSet xs in ps ++ [x:ys | ys <- ps]

qch3 = quickCheck((\xs -> (length xs < 10) ==> (length $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3' = verboseCheck((\xs -> (length xs < 10) ==> 
		(length $ nub $ powerSet xs) == (2^(length xs)))::[Int]->Property)

qch3'' = verboseCheck((\n -> (n >= 0 && n < 10) ==> 
		(length $ nub $ powerSet [1..n]) == (2^n))::Int->Property)

subset :: (Eq t) => [t] -> [t] -> Bool
subset xs ys = and [ elem x ys | x<-xs]

subset' :: (Eq t) => [t] -> [t] -> Bool
subset' xs ys = all  (\x -> elem x ys) xs

qch3''' = quickCheck((\n -> (n >= 0 && n < 10) ==> 
					    and [ subset m [1..n] | m <- powerSet [1..n] ])::Int->Property)

---------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia [] = id
kompozicia (f:fs) = (\x -> f (kompozicia fs x))

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' [] = id
kompozicia'' (f:fs) = \x -> f $ kompozicia'' fs x

qchf = quickCheck((\f -> \g -> \x -> (kompozicia [f,g]) x == (kompozicia'' [f,g]) x)::
			(Int->Int) -> (Int->Int) -> Int -> Bool)

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' [] x      = x
kompozicia''''' (f:fs) x  = f $ kompozicia''''' fs x

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' [] = id
kompozicia' (f:fs) = f . kompozicia' fs

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' [] = id
kompozicia'''' (f:fs) = kompozicia'''' fs . f

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = foldl (.) id

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = foldr (.) id

zoznamfcii = [(+7),(*11),(`mod` 1234567),(`div` 10),(^4),(+1),(*2),(^3)]

{-
*Main> kompozicia      zoznamfcii 1
95
*Main> kompozicia''    zoznamfcii 1
95
*Main> kompozicia''''' zoznamfcii 1
95
*Main> kompozicia'     zoznamfcii 1
95
*Main> kompozicia''''  zoznamfcii 1
550158565384
*Main> kompoziciaLeft  zoznamfcii 1
95
*Main> kompoziciaRight zoznamfcii 1
95

-- kompozicia funkcii nie je komutativna

*Main> kompozicia      (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''    (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''' (reverse zoznamfcii) 1
550158565384
*Main> kompozicia'     (reverse zoznamfcii) 1
550158565384
*Main> kompozicia''''  (reverse zoznamfcii) 1
95
*Main> kompoziciaLeft  (reverse zoznamfcii) 1
550158565384
*Main> kompoziciaRight (reverse zoznamfcii) 1
550158565384

-- evidentne definicia kompozicia'''' je zla, kedze predpokladala komutativnost (.)
-}
