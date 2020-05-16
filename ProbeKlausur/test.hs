d2 [] = []
d2 [_] = []
d2 (x : _ : xs) = x : d2 xs

list_1 = d2 . filter odd $ [0..]
list_2 = filter odd . d2 $ [0..]
list_3 = 1 : zipWith (+) list_3 (d2 [5..])

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq,Ord)
c1 = Node
--c2 = Node Nil $ True (Node Nil False Nil)
c3 = Node Nil 5 $ Node Nil 7 Nil
f x = if x > Nil then x else Nil