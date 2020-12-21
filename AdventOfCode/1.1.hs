import System.IO
import Control.Exception.Base
import Data.Char
parse:: String -> Int -> [Int] -> [Int]
parse [] _ ans = ans
parse (c:cs) acc ans = if c=='\n' then parse cs 0 (ans ++ [acc]) else parse cs (acc*10+ord(c)-ord('0')) ans

solveFirst_1::[Int] -> Int -> Int
solveFirst_1 lista termen = foldl (\y x -> if (x+termen==2020) then x*termen else y) 0 lista

solveFirst_1Helper::[Int] -> Int
solveFirst_1Helper (x:xs) = let ans = solveFirst_1 xs x
                            in if(ans/=0) then ans else solveFirst_1Helper xs

solve_1::Int -> Int -> [Int] -> Int
solve_1 primul doilea lista = foldl (\y x -> if(x+primul+doilea==2020) then x*primul*doilea else 0) 0 lista

solveSecond_1::Int -> [Int] -> Int
solveSecond_1 primul (y:ys) = let ans = solve_1 primul y ys
                              in if(ans/=0) then ans else solve_1 primul (head ys) (tail ys)

solveSecond_1Helper::[Int] -> Int
solveSecond_1Helper (x:xs) = let ans = solveSecond_1 x xs 
                             in if(ans/=0) then ans else solveSecond_1 (head xs) (tail xs)


main:: IO()

main = do input <- readFile "/home/fbi/Desktop/AdventOfCode/input.txt"
          let lista = parse input 0 []
          print $ solveFirst_1Helper lista
             



