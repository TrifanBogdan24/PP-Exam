module Exam where

data BTree = Empty | Node Int BTree BTree deriving (Show, Eq)

{--
 - 1.
 - Fiind dat un arbore binar, sa se implementeze functia mirror care inverseaza
 - subarborii fiecarui nod.
 -
 - e.g.
 -      1                   1
 -     / \                 / \
 -    /   \               /   \
 -   3     4  -mirror->  4     3
 -  / \     \           /     / \
 - 5   6     7         7     6   5
 -}



mirror :: BTree -> BTree
mirror = undefined


{--
 - 2.
 - Implementati functia get_level care intoarce o lista cu toate elementele 
 - de pe nivelul n al unui arbore binar.
 -}

get_level :: Int -> BTree -> [Int]
get_level = undefined

-- daca primul paremetru este 0, atunci am ajuns la un nod de pe nivelul `n`
get_level = undefined

-- `++` operatorul de concatenare a doua liste
get_level = undefined

{--
 - 3.
 - Pornind de la recurenta: f(n) = f(n-1) + f(n-4)
 - f(0) = 1, f(1) = 1, f(2) = 1, f(3) = 1
 - 
 - Definiti lista infinita care contine toate valorile functiei f.
 -}




f :: [Int]
f = undefined


{--
 - Zipper este un tip de date care reprezinta o lista cu un element curent.
 -   [1 2 3] 4 [5 6 7] reprezinta un zipper cu elementul curent 4
 -
 - Aveti grija ca reprezentarea interna o sa fie:
 -  (Zipper [3, 2, 1] 4 [5, 6, 7]) deoarece head-ul listei din stanga trebuie sa fie
 -  cel mai apropiat element de elementul curent.
 -}

data Zipper a = Zipper [a] a [a] deriving Show

{--
 - 4.
 - Implementati functia shift care primeste o directie si un zipper si intoarce 
 - un zipper care are elementul curent mutat in directia respectiva.
 -
 - e.g.
 - [1 2 3] 4 [5 6 7] -> shift L -> [2 3] 1 [4 5 6 7]
 - [1 2 3] 4 [5 6 7] -> shift R -> [1 2 3 4] 5 [6 7]
 -}

data Direction = L | R

shift ::  Direction -> Zipper a -> Zipper a
shift = undefined


{--
 - 5.
 - Inrolati tipul Zipper in clasa Eq astfel incat sa puteti compara doua zipere.
 - 2 zipere sunt egale daca listele lor sunt egale indiferent de elementul curent.
 -
 - e.g.
 - [1 2 3] 4 [5 6 7] == [1 2 3] 4 [5 6 7] -> True
 - [1 2] 3 [4 5 6 7] == [1 2 3] 4 [5 6 7] -> True
 - [1 2 3] 4 [5 6 7] == [1 2 3] 5 [6 7] -> False
 -}

instance (Eq a) => undefined

{--
 - 6. [BONUS]
 - Implementati functia map_all care transforma un zipper, aplicand o functie 
 - pe fiecare element al listei ca si cum ar fi elementul curent.
 -
 - f left x right = sum left + x
 - [1 2 3] 4 [5 6 7] -> map_all f -> [1 3 6] 10 [15 21 28]
 -}


-- NU TREC TESTELE!!!!!!!
map_all :: ([a] -> a -> [a] -> b) -> Zipper a -> Zipper b
map_all = undefined

-- Helper functions
tails :: [a] -> [[a]]
tails = undefined
