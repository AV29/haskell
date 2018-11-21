doubleMe x = x + x

doubleUs x y  = doubleMe x + doubleMe y

doubleSamllNumber x = if x > 100 then x else x*2

replacer list = [if x <= 5 then 'A' else 'Z' | x <- list, x `mod` 2 == 0]

twoListsProductionFiltered list1 list2 = [x*y | x <- list1, y <- list2, x < 50]

customLength list = sum [1 | _ <- list]

trimLowerCaseLetters string = [c | c <- string, c `elem` ['A'..'Z']]

leftEvensInNestedLists wrapperList = [[x | x <- nestedList, even x] | nestedList <- wrapperList]