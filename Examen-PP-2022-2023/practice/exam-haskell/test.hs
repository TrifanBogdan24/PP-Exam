import Control.Exception
import System.IO.Unsafe
import Exam

--- type declaration ---

data Test a = Test {
    state :: Bool,
    info :: IO a
}

instance Monad Test where
    return x = Test True (return x)
    p >>= f = Test (state p && state q) (info p >>= (\_ -> info q))
        where q = f undefined

instance Applicative Test where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return $ f x

instance Functor Test where
    fmap f px = do
        x <- px
        return $ f x

--- type declaration over ---

test :: String -> Bool -> Test ()
test msg True = Test True $ putStrLn $ "\ESC[32m" ++ "[PASSED]" ++ "\ESC[0m " ++ msg
test msg False = Test False $ putStrLn $ "\ESC[91m" ++ "[FAILED]" ++ "\ESC[0m " ++ msg

test_eq :: (Show a, Eq a) => String -> a -> a -> Test ()
test_eq msg a b | a == b = test msg True
                | a /= b = Test False $ do
                      info $ test msg False
                      putStrLn $ "expected: " ++ (show b)
                      putStrLn $ "found: " ++ (show a)

test_suite :: String -> Test () -> Integer -> IO ()
test_suite name tests points = catch (f tests) handle 
  where 
    f (Test True test_io) = do
        putStrLn $ "\ESC[32m" ++ "[PASSED SUITE]" ++ "\ESC[0m " ++ name 
        test_io
        putStrLn $ "+" ++ (show points) ++ " points"
    f (Test False test_io) = do
        putStrLn $ "\ESC[91m" ++ "[FAILED SUITE]" ++ "\ESC[0m " ++ name
        test_io
    handle (ErrorCall e) = putStrLn $ "\ESC[91m" ++ "Error" ++ " \ESC[0mat " ++ name ++ ": " ++ e

--- tests ---

l1 = Nest [Val 1, Nest [Val 2, Val 3], Val 5, Nest [Val 0, Val 1]]
l2 = Nest [Val 2, Nest [Val 3, Val 4], Val 6, Nest [Val 1, Val 2]]
l3 = Nest [Val 1, Nest [Val 2, Nest [Val 3]], Nest [Val 4, Val 5]]

ex1 :: Test ()
ex1 = do
    test_eq "examplu" (nestedFilter (>2) l1) (Nest [Nest [Val 3], Val 5])
    test_eq "lista vida" (nestedFilter (<2) l1) (Nest [Val 1, Nest [Val 0, Val 1]])
    test_eq "empty" (nestedFilter (>2) (Nest [])) (Nest [])

ex2 :: Test ()
ex2 = do
    test_eq "functor id" (fmap id l1) (id l1)
    test_eq "functor asociativ" (fmap (f . g) l1) (fmap f $ fmap g l1)
    test_eq "corectitudine" (fmap f l1) l2
  where 
    f = (+1)
    g = (*2)

ex3 :: Test ()
ex3 = do
    test_eq "exemplu" (depth l3) 3
    test_eq "lista vida" (depth (Nest [])) 1
    test_eq "lista cu o valoare" (depth (Nest [Val 1])) 1

ex4 :: Test ()
ex4 = do
    test_eq "clean Int" (clean [Left 1, Left 2, Left 3]) (Just $ Left [1, 2, 3])
    test_eq "clean String" (clean [Right "a", Right "b", Right "c"]) (Just $ Right ["a", "b", "c"])
    test_eq "empty" (clean []) Nothing 
    test_eq "mixed" (clean [Left 1, Right "a", Left 2, Right "b"]) Nothing


ex5 :: Test ()
ex5 = do
    test_eq "Stanga egal" (s1 == s1) True
    test_eq "Stanga diferit" (s1 == s2) False
    test_eq "Dreapta egal" (d1 == d1) True
    test_eq "Dreapta diferit" (d1 == d2) False
    test_eq "Stanga Dreapta diferit" (s1 == d1) False
    test_eq "Dreapta Stanga diferit" (d1 == s1) False
  where
    s1 = Stanga [1, 2, 3] :: Which Int String 
    s2 = Stanga [1, 2, 4] :: Which Int String
    d1 = Dreapta ["a", "b", "c"] :: Which Int String
    d2 = Dreapta ["a", "b", "d"] :: Which Int String


ex6 :: Test ()
ex6 = do
    test_eq "arbore simplu" (checkEq [1, 2, 3] t1) True
    test_eq "arbore simplu" (checkEq [1, 2, 4] t1) False
    test_eq "arbore complex" (checkEq [1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3] t2) True
  where
    t1 = Node (Node (Node Void 3 Void) 2 Void) 1 Void
    t2 = Node t1 0 (Node t1 0 t1)

main :: IO ()
main = do
    test_suite "exercitiu 1" ex1 1
    test_suite "exercitiu 2" ex2 1
    test_suite "exercitiu 3" ex3 1
    test_suite "exercitiu 4" ex4 1
    test_suite "exercitiu 5" ex5 1
    test_suite "exercitiu 6" ex6 1
