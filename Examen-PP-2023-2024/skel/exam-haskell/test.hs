import Control.Exception (catch, ErrorCall(..))
import Data.List (sort)

import Exam

data TestCase a = Test {
    info :: String,
    passed :: Bool,
    state :: a
}

instance Monad TestCase where
    return x = Test "" True x
    x >>= f = Test ninfo npassed nstate
      where
        y = f (state x)
        ninfo = info x ++ newline ++ info y
        npassed = passed x && passed y
        newline = if info y == "" then "" else "\n"
        nstate = state y

instance Applicative TestCase where
    pure = return
    f <*> x = do
        g <- f
        y <- x
        return (g y)

instance Functor TestCase where
    fmap f x = do
        y <- x
        return (f y)

red :: String -> String
-- red msg = msg -- uncomment this line to disable color
red msg = "\x1b[31m" ++ msg ++ "\x1b[0m"

green :: String -> String
-- green msg = msg -- uncomment this line to disable color
green msg = "\x1b[32m" ++ msg ++ "\x1b[0m"

assert :: String -> Bool -> TestCase Bool
assert msg cond = Test info cond cond
  where
    info = (if cond then green "[PASSED] " else red "[FAILED] ") ++ msg

assert_eq :: (Eq a, Show a) => String -> a -> a -> TestCase Bool
assert_eq msg actual expected = assert aug_msg (expected == actual)
  where
    aug_msg = msg ++ error_msg
    error_msg = if expected == actual 
                then ""
                else "\nExpected: " ++ show expected ++ 
                     "\n  Actual: " ++ show actual

assert_count_eq :: (Ord a, Show a) => String -> [a] -> [a] -> TestCase Bool
assert_count_eq msg actual expected = assert_eq msg sactual sexpected
  where
    sactual = sort actual
    sexpected = sort expected

assert_neq :: (Eq a, Show a) => String -> a -> a -> TestCase Bool
assert_neq msg actual expected = assert msg (expected /= actual)

assert_err :: String -> Either String a -> TestCase Bool
assert_err msg (Left _) = assert msg True
assert_err msg (Right _) = assert msg False

section :: String -> TestCase ()
section msg = Test msg True ()

eval :: TestCase a -> IO Int
eval test = catch (f test) handler
  where
    f :: TestCase a -> IO Int
    f test = do
        putStrLn $ info test
        return $ if passed test then 1 else 0

    handler :: ErrorCall -> IO Int
    handler (ErrorCall msg) = do
        putStrLn $ red $ "\n[ERROR] " ++ msg
        return 0

runTests :: [TestCase a] -> IO Int
runTests tests = do
    evals <- mapM eval tests
    return $ sum evals

exTest1 :: TestCase Bool
exTest1 = do
    let tree1 = Node 1 (Node 3 (node 5) (node 6)) (Node 4 Empty (node 7))
    let tree2 = Node 1 (Node 4 (node 7) Empty) (Node 3 (node 6) (node 5))

    let tree3 = Node 1 tree1 (Node 2 tree2 tree1)
    let tree4 = Node 1 (Node 2 tree2 tree1) tree2

    section "==== 1. mirror ===="
    assert_eq "exemplu" (mirror tree1) tree2 
    assert_eq "exemplu invers" (mirror tree2) tree1
    assert_eq "exemplu complex" (mirror tree3) tree4
    assert_eq "exemplu complex invers" (mirror tree4) tree3
    assert_eq "arbore gol" (mirror Empty) Empty
  where
    node x = Node x Empty Empty

exTest2 :: TestCase Bool
exTest2 = do
    let tree1 = Node 1 (Node 3 (node 5) (node 6)) (Node 4 Empty (node 7))

    section "==== 2. get_level ===="
    assert_eq "get_level 0" (get_level 0 tree1) [1]
    assert_eq "get_level 1" (get_level 1 tree1) [3, 4]
    assert_eq "get_level 2" (get_level 2 tree1) [5, 6, 7]
    assert_eq "get_level 3" (get_level 3 tree1) []
    assert_eq "get_level 0 arbore gol" (get_level 0 Empty) []
    assert_eq "get_level 1 arbore gol" (get_level 1 Empty) []
  where
    node x = Node x Empty Empty

exTest3 :: TestCase Bool
exTest3 = do
    section "==== 3. f ===="
    assert_eq "initializare" (take 4 f) [1, 1, 1, 1]
    assert_eq "primele 10 elemente" (take 10 f) [1, 1, 1, 1, 2, 3, 4, 5, 7, 10]
    assert_eq "elementul 100" (f !! 100) 54327237184326

exTest4 :: TestCase Bool
exTest4 = do
    let zipper = Zipper [3, 2, 1] 4 [5, 6, 7]
    let zipperL = Zipper [2, 1] 3 [4, 5, 6, 7]
    let zipperR = Zipper [4, 3, 2, 1] 5 [6, 7]
    let zipperL2 = Zipper [] 1 [2, 3, 4, 5, 6, 7]
    let zipperR2 = Zipper [1, 2, 3, 4, 5, 6] 7 []

    section "==== 4. shift ===="
    assert "exemplu shift L" $ eq (shift L zipper) zipperL
    assert "exemplu shift R" $ eq (shift R zipper) zipperR
    assert "shift L de la inceput" $ eq (shift L zipperL2) zipperL2
    assert "shift R de la sfarsit" $ eq (shift R zipperR2) zipperR2
  where
    eq (Zipper l1 c1 r1) (Zipper l2 c2 r2) = l1 == l2 && c1 == c2 && r1 == r2

exTest5 :: TestCase Bool
exTest5 = do
    let zipper = Zipper [3, 2, 1] 4 [5, 6, 7]
    let zipperL = Zipper [2, 1] 3 [4, 5, 6, 7]
    let zipperR = Zipper [5, 4, 3, 2, 1] 6 [7]
    let zipper2 = Zipper [3, 2, 1] 5 [6, 7]

    section "==== 5. Eq Zipper ===="
    assert_eq "exemplu 1" (zipper == zipper) True
    assert_eq "exemplu 2" (zipper == zipperL) True
    assert_eq "exemplu 3" (zipper == zipper2) False
    assert_eq "shift R" (zipper == zipperR) True
    assert_eq "shift R vs shift L" (zipperR == zipperL) True
    assert_eq "shift L vs shift R (comutativitate)" (zipperL == zipperR) True
    assert_eq "False" (zipperR == zipper2) False

exTest6 :: TestCase Bool
exTest6 = do
    let zipper = Zipper [3, 2, 1] 4 [5, 6, 7]
    let zipperL = Zipper [2, 1] 3 [4, 5, 6, 7]

    let fzipper = Zipper [6, 3, 1] 10 [15, 21, 28]
    let fzipperL = Zipper [3, 1] 6 [10, 15, 21, 28]

    let gzipper = Zipper [25, 27, 28] 22 [18, 13, 7]
    let gzipperL = Zipper [27, 28] 25 [22, 18, 13, 7]

    let hzipper = Zipper [5, 3, 2] 10 [29, 126, 727]
    let hzipperL = Zipper [3, 2] 5 [10, 29, 126, 727]

    let wzipper = Zipper [843, 2522, 5041] 214 [47, 13, 8]
    let wzipperL = Zipper [2522, 5041] 843 [214, 47, 13, 8]

    section "==== 6. [BONUS] map_all ===="
    assert "exemplu 1" $ eq (map_all f zipper) fzipper
    assert "exemplu 1 shift L" $ eq (map_all f zipperL) fzipperL
    assert "right sum" $ eq (map_all g zipper) gzipper
    assert "right sum shift L" $ eq (map_all g zipperL) gzipperL
    assert "left product" $ eq (map_all h zipper) hzipper
    assert "left product shift L" $ eq (map_all h zipperL) hzipperL
    assert "right product" $ eq (map_all w zipper) wzipper
    assert "right product shift L" $ eq (map_all w zipperL) wzipperL
  where
    f l x r = sum l + x
    g l x r = sum r + x
    h l x r = (foldl (*) 1 l) + x
    w l x r = (foldl (*) 1 r) + x
    eq (Zipper l1 c1 r1) (Zipper l2 c2 r2) = l1 == l2 && c1 == c2 && r1 == r2

main :: IO ()
main = do
    let tests = [ exTest1
                , exTest2
                , exTest3
                , exTest4
                , exTest5
                , exTest6
                ]
    total <- runTests tests
    putStrLn $ "Total: " ++ (show total) ++ " points"
