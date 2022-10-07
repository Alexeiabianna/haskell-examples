data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

buscar :: Eq a => a -> Tree a -> Bool
buscar x (Leaf y) = x == y
buscar x (Node l y r) = x || buscar x l || buscar x r

achatar :: Tree a -> [a]
achatar (Leaf x) = [x]
achatar (Node l x r) = achatar l ++ [x] ++ achatar r

data ArvBin a = Vazia | Nodo (ArvBin a) a (ArvBin a) deriving