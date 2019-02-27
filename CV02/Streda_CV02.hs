module Priprava_CV02 where
import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)

-- definujte predikat pre usporiadany/rastuci/nerastuci zoznam
ordered :: [Int] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:xs) = x<=y && ordered (y:xs) 

ordered' [] = True
ordered' xs = all (\(p,d)->p<=d) $zip (init xs)(tail xs)

qch3 = quickCheck(\xs -> ordered xs == ordered' xs)

-- definujte a overte nejaku vlastnost funkcie ordered
qch1 = quickCheck( \xs -> ordered xs ==(xs==sort xs) ) -- quickCheck( \xs -> cond ==> proposition )
qch1' = quickCheck( \xs -> ordered (sort xs) )
qch1v = undefined -- verboseCheck( \xs -> cond ==> proposition )

-- naprogramuje jeden prechod bubble sort algoritmu
bubble :: [Int] -> [Int]
bubble [] = [] 
bubble [x] = [x] 
bubble (x:y:xs) = if x>y then y:bubble (x:xs) else x:bubble (y:xs)

qch4 = quickCheck( \xs -> (xs/=[]) ==>(maximum xs) == last (bubble xs) )
qch5 = quickCheck( \xs -> length (xs) == length (bubble xs) )

-- pouzite bubble na bubbleSort
bubbleSort  :: [Int]->[Int]
bubbleSort xs = (iterate bubble xs)!!(length xs)
e = bubbleSort [4,3,4,6,7,4,3,1,1,2,3,4,5,6,7,8,9,0,5,3,2,3,2,3,4,5,6,7,1,2,2,0,9,12,11]
--[0,0,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,6,7,7,7,8,9,9,11,12]

qch6 = quickCheck( \xs -> ordered $bubbleSort xs )
qch7 = quickCheck( \xs -> bubbleSort xs == sort xs )

-- definujte a overte nejaku vlastnost funkcie bubbleSort
qch2 = undefined -- quickCheck( \xs -> cond ==> proposition )
qch2v = undefined -- verboseCheck( \xs -> cond ==> proposition )

---------------------------------
-- kompozícia zoznamu funkcií,  :: [a->a] -> (a->a)

-- zaciatocnicka definicia cez zoznamovu rekurziu
kompozicia  :: [a->a] -> (a->a)
kompozicia [] = id
kompozicia [f] = f
--kompozicia (f:fs) = f . (kompozicia fs)
--kompozicia (f:fs) = \x -> f $kompozicia fs x 
kompozicia xs = foldr (.) id xs 
 

-- definicia haskellistu, co si nasiel operator $
kompozicia''  :: [a->a] -> (a->a)
kompozicia'' = undefined

-- definicia haskellistu, co si este prehodil x na lavu stranu
kompozicia'''''  :: [a->a] -> (a->a)
kompozicia''''' = undefined

-- jemne pokrocily haskellista, ktory bol na prednaske
kompozicia'  :: [a->a] -> (a->a)
kompozicia' = undefined

-- haskellista, co si pamata, ze skladanie funkcii je asociativne ale nepamata, ze nie je komutativne
kompozicia''''  :: [a->a] -> (a->a)
kompozicia'''' = undefined

-- haskellista, co bude volit lavicu
kompoziciaLeft  :: [a->a] -> (a->a)
kompoziciaLeft = undefined

-- haskellista, co bude volit neexistujucu pravicu
kompoziciaRight  :: [a->a] -> (a->a)
kompoziciaRight = undefined

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
