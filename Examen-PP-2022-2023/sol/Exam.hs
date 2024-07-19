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
nestedFilter f (Val x) = if f x then Val x else Nest []
nestedFilter f (Nest []) = Nest []
nestedFilter f (Nest (x : xs)) = Nest (nestedFilter f x : nestedFilterList f xs)
    where
        nestedFilterList :: (a -> Bool) -> [NestedList a] -> [NestedList a]
        nestedFilterList f [] = []
        nestedFilterList f (x : xs) = nestedFilter f x : nestedFilterList f xs

{-
    2.  [Mediu]
        Inrolati NestedList in clasa Functor
-}

instance Functor NestedList where
    fmap f (Val x)= Val (f x)
    fmap f (Nest (x : xs)) = Nest (fmap f x : fmapList f xs ) 
        where 
            fmapList :: (a -> b) -> [NestedList a] -> [NestedList b]
            fmapList f [] = []
            fmapList f (x : xs) = fmap f x : fmapList f xs

{-
    3.  [Usor]
        Definiti o functie care determina adancimea (nivelul maxim de imbricare)
        al unei liste.
        Exemplu, adancimea listei (1 (2 (3)) (4 5)) este 3.
-}

depth :: NestedList a -> Int
depth (Val x) = 0
depth (Nest []) = 1
depth (Nest (x : xs)) = 1 + max (depth x) (depthList xs)
    where 
        depthList :: [NestedList a] -> Int
        depthList [] = 0
        depthList (x : xs) = max (depth x) (depthList xs)

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
clean [] = Nothing
clean (x : xs) = case x of
    Left i -> case clean xs of
        Nothing -> Just (Left [i])
        Just (Left is) -> Just (Left (i : is))
        Just (Right ss) -> Nothing
    Right s -> case clean xs of
        Nothing -> Just (Right [s])
        Just (Left is) -> Nothing
        Just (Right ss) -> Just (Right (s : ss))

{-
    5. Inrolati tipul Which in clasa Eq:
-}

data Which a b = Stanga [a] | Dreapta [b]

instance (Eq a, Eq b) => Eq (Which a b) where
    (==) (Stanga x) (Stanga y) = x == y
    (==) (Dreapta x) (Dreapta y) = x == y
    (==) _ _ = False

{-
    6. Fie un arbore definit astfel.
       Implementati o functie care verifica daca toate elementele
       din lista se regasesc in arbore si vice-versa.
-}

data Tree a = Void | Node (Tree a) a (Tree a)

checkEq :: Ord a => [a] -> Tree a -> Bool
checkEq [] Void = True
checkEq [] _ = False
checkEq (x : xs) Void = False
checkEq (x : xs) (Node left y right) = if x == y then checkEq xs (Node left y right) else checkEq (x : xs) left || checkEq (x : xs) right
