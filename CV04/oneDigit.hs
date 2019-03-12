module Abaku where
import Data.List
import Data.Char


oneDigit numb digit  = [ expr | n <- [1..],
                                expr <- expressions $ take n $ repeat (chr (digit+48)),
                                eval expr == numb
                       ]
        
asc numb  = [ expr | expr <- expressions "123456789",
                     eval expr == numb
            ]
                  
desc numb = [ expr | expr <- expressions "987654321",
                     eval expr == numb
            ]
                                  
                                  
data Tree = Leaf Int | Op Int Tree Tree  
 -- deriving Show

wrap :: Tree -> String
wrap t@(Op 1 _ _)        = "(" ++ show t ++ ")"
wrap t@(Op 2 _ _)        = "(" ++ show t ++ ")"
wrap t                                 = show t

instance Show Tree where
  show (Leaf x) = show x
  show (Op 0 left right) = (show left) ++ (show right)    
  show (Op 1 left right) = (show left) ++ "+" ++ (show right)
  show (Op 2 left right@(Leaf _)) = (show left) ++ "-" ++ (show right)
  show (Op 2 left right@(Op 0 _ _ )) = (show left) ++ "-" ++ (show right)
  show (Op 2 left right) = (show left) ++ "-(" ++ (show right) ++ ")"
  show (Op 3 left right) = (wrap left) ++ "*" ++ (wrap right)
  show (Op 4 left right) = (wrap left) ++ "/" ++ (wrap right)

isCorrect        :: Tree -> Bool
isCorrect (Op 1 (Leaf 0) _)  = False
isCorrect (Op 2 (Leaf 0) _)  = False
isCorrect (Op 3 (Leaf 0) _)  = False
isCorrect (Op 4 (Leaf 0) _)  = False
isCorrect (Op 1 _ (Leaf 0))  = False
isCorrect (Op 2 _ (Leaf 0))  = False
isCorrect (Op 3 _ (Leaf 0))  = False
isCorrect (Op 4 _ (Leaf 0))  = False
isCorrect (Op 0 (Leaf _) (Leaf _))  = True
isCorrect (Op 0 (Leaf _) right@(Op 0 _ _))  = isCorrect right
isCorrect (Op 0 _ _)  = False
isCorrect (Op _ left right)  = (isCorrect left) && (isCorrect right)
isCorrect (Leaf _)  = True


expressions :: [Char] -> [Tree]
expressions [] = []
expressions [x] = [Leaf (ord x-ord '0')]
expressions list = [ Op op left right |
                    i<-[1..length(list)-1],
                    op <- [1,2,3,4,0], -- join, plus, minus, krat, deleno
                    left <- expressions (take i list),
                                        -- eval left < 10000,
                    right <- expressions (drop i list),
                                        -- eval right < 10000,
                    isCorrect (Op op left right)
                    ]
               
eval  :: Tree -> Int               
eval (Leaf x) = x                    
eval (Op 0 left right) = 10*(eval left)+(eval right)
eval (Op 1 left right) = (eval left)+(eval right)
eval (Op 2 left right) = (eval left)-(eval right)
eval (Op 3 left right) = (eval left)*(eval right)
eval (Op 4 left right) = if r == 0 then 9999999999 else (eval left) `div` (eval right)
                         where r = (eval right)



              