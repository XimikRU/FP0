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



