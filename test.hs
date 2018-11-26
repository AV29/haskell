doubleMe :: (Num x) => x -> x
doubleMe x = x + x

doubleUs :: (Num x) => x -> x -> x
doubleUs x y  = doubleMe x + doubleMe y

doubleSamllNumber x = if x > 100 then x else x*2

replacer list = [if x <= 5 then 'A' else 'Z' | x <- list, x `mod` 2 == 0]

twoListsProductionFiltered list1 list2 = [x*y | x <- list1, y <- list2, x < 50]

customLength :: [a] -> Int
customLength list = sum [1 | _ <- list]

trimLowerCaseLetters string = [c | c <- string, c `elem` ['A'..'Z']]

leftEvensInNestedLists wrapperList = [[x | x <- nestedList, even x] | nestedList <- wrapperList]

findRightTriangles = [(a, b, c) | c <- [1..500], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 1000]

factorial::(Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x - 1)

fibbonachi::(Integral a) => a -> a
fibbonachi 0 = 0
fibbonachi 1 = 1
fibbonachi x = fibbonachi(x-1) + fibbonachi(x-2)

addVectors::(Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a + c, b + d)

customSum::(Num a) => [a] -> a
customSum [] = 0
customSum (x:rest) = x + customSum rest

bmiTell::(RealFloat a) => a -> a-> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"

customMax::(Ord a) => a -> a -> a
customMax a b
    | a > b = a
    | otherwise  = b

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
     where (f:_) = firstname
           (l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

recursiveMax :: (Ord a) => [a] -> a
recursiveMax [] = error "Maximum of an empty list"
recursiveMax [x] = x
recursiveMax (x:tail) = max x (recursiveMax tail)

recursiveReplicate :: (Ord n, Num n) => n -> a -> [a]
recursiveReplicate n x
    | n <= 0  = []
    | otherwise  = x:recursiveReplicate (n - 1) x

recursiveTake :: (Ord n, Num n) => n -> [a] -> [a]
recursiveTake n _ | n <= 0 = []
recursiveTake _ [] = []
recursiveTake n (x:tail) = x:recursiveTake (n-1) tail

recursiveReverse :: [a] -> [a]
recursiveReverse [] = []
recursiveReverse (x:tail) = recursiveReverse tail ++ [x]

recursiveRepeat :: a -> [a]
recursiveRepeat x = x: recursiveRepeat x

customRepeat :: Int -> val -> [val]
customRepeat count value = take count (recursiveRepeat value)

recursiveZip :: [a] -> [b] -> [(a,b)]
recursiveZip _ [] = []
recursiveZip [] _ = []
recursiveZip (x:xs) (y:ys) = (x,y):recursiveZip xs ys


recursiveElem :: (Eq a) => a -> [a] -> Bool
recursiveElem val [] = False
recursiveElem val (x:tail) |  x == val = True | otherwise = val `recursiveElem` tail

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (current:tail) =
    let smaller = quicksort [a | a <- tail, a <= current]
        bigger = quicksort [a | a <- tail, a > current]
    in smaller ++ [current] ++ bigger
