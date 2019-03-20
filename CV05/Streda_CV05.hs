module Cvicenie5 where

import Data.List
import Data.Char
import Terms

instance Show LExp where
  show (LAMBDA v e) = '\\' : v ++ "->" ++ show e
  show (ID v) = v
  show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

foldLambda :: (String -> t -> t) -> (String -> t) -> (t -> t -> t) -> LExp -> t
foldLambda lambda var apl (LAMBDA str exp)  = lambda str (foldLambda lambda var apl exp)
foldLambda lambda var apl (ID str)          = var str
foldLambda lambda var apl (APP exp1 exp2)   = apl (foldLambda lambda var apl exp1) 
                                                  (foldLambda lambda var apl exp2)

vars  :: LExp -> [String]
vars = undefined

show' :: LExp -> String
show' = undefined
                                                  
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
--ithree =  (APP (APP iplus itwo) ione)
--inine =   (APP (APP itimes ithree) ithree)
--isixteen = (APP (APP ipower itwo) ifour)

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))

-- iplus =  fromString "?m.?n.?f.?x.((m f) ((n f) x))" 
-- itimes = fromString "?m.?n.?f.?x.((m (n f)) x)"
-- ipower = fromString "?m.?n.(n m)"  

-- najst vsetky podtermy termu
-- priamociaro
podtermy :: LExp -> [LExp]
podtermy = nub . podtermy'

podtermy' :: LExp -> [LExp]
podtermy'  = undefined

{--
podtermy (LAMBDA "x" (APP (ID "x") (ID "x")))
[\x->(x x),(x x),x]
--}

-- akumulatorom
podtermy1 :: LExp -> [LExp]
podtermy1 = nub . (podtermy1' [])

podtermy1' :: [LExp] -> LExp -> [LExp]
podtermy1' = undefined


-- nahradi premennu za premennu
nahrad ::  LExp -> String -> String -> LExp
nahrad = undefined

{-
nahrad (LAMBDA "x" (APP (ID "x") (ID "x"))) "x" "y"
\x->(x x)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "z"
\x->(x z)
nahrad (LAMBDA "x" (APP (ID "x") (ID "y"))) "y" "x"
\x->(x x)
:-(
-}

fromString  :: String -> (LExp, String)
fromString (x:xs)   | isAlpha x  = (ID [x], xs)
                    | x == '('  = let (exp1, rest) = fromString xs in
                                                                                  let (exp2, nrest) = fromString (tail rest) in 
                                                                                            (APP exp1 exp2, tail nrest)
                    | x == '\\' = let (exp, rest) = fromString (drop 3 xs) in
                                                                                            (LAMBDA [head xs] exp, rest)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
 

free    :: String -> LExp -> Bool
free   = undefined

