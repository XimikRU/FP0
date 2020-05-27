-- Списки: [1, 6, 14, 17, 18]
-- Коды: [2]
-- Деревья: [4]

module Main where

-- Задача 1
-- Определите функцию, возвращающую последний элемент списка.

newLast inputList = 
    if null(init(inputList)) then 
        head(inputList) 
    else 
        newLast(tail(inputList))

-- Тесты

main = do
    putStrLn "Задача 1"
    let inputList = [1,2,3,4,5,7,8,9,0]
    print("Input: [1,2,3,4,5,7,8,9,0]")
    print(newLast(inputList))

    let inputList = [1]
    print("Input: [1]")
    print(newLast(inputList))



-------------------------------------------------------------
-------------------------------------------------------------
-------------------------- ДЕБАГ ----------------------------
-------------------------------------------------------------
-------------------------------------------------------------

{-

-- Задача 1
-- Определите функцию, возвращающую последний элемент списка.

newLast [x] = x
newLast [] = error "Empty list!"
newLast inputList = 
    if null(init(inputList)) then 
        head(inputList) 
    else 
        newLast(tail(inputList))

-- Задача 6
-- Распаковка [(4,'a'),(1,'b')] в ['a','a','a','a','b']

-- TODO: как
extractList inputList = 
    map(\inputList -> (length inputList, head inputList)) (group inputList)

-- Тесты

logger task test input = 
    print task
    print test
    print input

main = do
    print "Задача 1"

    let inputList = [1,2,3,4,5,7,8,9,0]
    logger "#1" "#1: Полный список" inputList

    let inputList = []
    logger "#1" "#1: Пустой спсикок" inputList

    let inputList1 = [1]
    logger "#1" "#1: Список из 1 элемента" inputList

-}
