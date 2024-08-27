module Exam where

import Data.List

{-
    Urmatorul tip de date codifica liste ce pot
    contine ca elemente liste imbricate.

    Considerati faptul ca constructorul Val poate aparea doar in interiorul
    unei liste.
-}

data NestedList a = Val a | Nest [NestedList a] 
    deriving (Show, Eq) -- pentru testare

{-
    1. [Mediu] 
       Implementati o functie filter peste NestedList.
       Listele vide imbricate ce pot aparea vor fi eliminate.

       nestedFilter (>2)  (1 (2 3) 5 (0 1)) = ((3) 5)
-}

nestedFilter :: (a -> Bool) -> NestedList a -> NestedList a

{-
    2.  [Mediu]
        Inrolati NestedList in clasa Functor
-}


{-
    3.  [Usor]
        Definiti o functie care determina adancimea (nivelul maxim de imbricare)
        al unei liste.
        Exemplu, adancimea listei (1 (2 (3)) (4 5)) este 3.
-}

depth :: NestedList a -> Int

{-
    4.  [Mediu]
        Scrieti o functie ce primeste ca parametru o lista de valori, din care fiecare
        poate fi un Int (Left i) sau un String (Right s).
        Daca TOATE valorile sunt Int, functia va construi lista lor
        Daca TOATE valorile sunt String, din nou, functia va construi lista lor
        Daca valorile sunt amestecate, sau lista este vida, functia va intoarce Nothing

        data Either a b = Left a | Right b
        data Maybe a = Nothing | Just a
-}

clean :: [Either Int String] -> Maybe (Either [Int] [String])

{-
    5. Inrolati tipul Which in clasa Eq:
-}

data Which a b = Stanga [a] | Dreapta [b]



{-
    6. Fie un arbore definit astfel.
       Implementati o functie care verifica daca toate elementele
       din lista se regasesc in arbore si vice-versa.
-}

data Tree a = Void | Node (Tree a) a (Tree a)
