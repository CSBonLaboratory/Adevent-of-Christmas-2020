import System.IO
import Control.Exception.Base
import Data.ByteString.Char8
import Data.Char
import Data.Bits
parse:: String->[ByteString]
parse input = split '\n' (pack input)

solve:: [ByteString] -> Int -> Bool -> Int
solve [] acc _ = acc
solve (x:xs) acc temp = let f = (\field -> if (Prelude.take 3 field) == "cid" then True else Prelude.foldl (\y x -> if (Prelude.take 3 field) == x then (.|.) y True else (.|.) y False) False ["byr","iyr","eyr","hgt","hcl","ecl","pid"]) 
                        in if Prelude.null (unpack x) == True
                            then if temp == True
                                  then solve xs (acc +1) True
                                 else solve xs acc True
                           else solve xs acc ((.&.) temp (f (unpack(x))))


    


main:: IO()
main = do input <- System.IO.readFile "/home/fbi/Desktop/AdventOfCode/input.txt"
          let info = parse input
          print $ solve info 0 True