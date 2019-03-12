module Huffman where

data HTree = Leaf (Int, String) | Node HTree Int HTree deriving (Show, Eq)

instance Ord HTree where 
                        t1 < t2 = weight t1 < weight t2
                        t1 <= t2 = weight t1 <= weight t2
                --      t1 == t2 = weight t1 == weight t2

weight :: HTree -> Int
weight (Leaf (n, s)) = n
weight (Node l n r) = n       

-- vsun prvok a::t do utriedeneho zoznamu xs::[t]
insert :: (Ord t) => t -> [t] -> [t]
insert a xs = undefined

-- zober najmensie dva prvky (prve dva), urob z nich HTree a vsun do zoznamu...
combine :: [HTree] -> [HTree]
combine (x:y:xs) = undefined

isSingle :: [HTree] -> Bool
isSingle xs = length xs == 1

-- naprv vyrob (map Leaf ft), opakuj combine, ak kym neplati isSingle...
huffman :: [(Int, String)] -> HTree
huffman ft = undefined