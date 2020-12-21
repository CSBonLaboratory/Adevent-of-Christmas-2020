import System.IO
import Control.Exception.Base
import Data.ByteString.Char8
import Data.Char

breakLines:: String -> [ByteString]
breakLines input = split '\n' (pack input)


parse::[ByteString] -> ([Int],[Int],[Char],[String]) -> ([Int],[Int],[Char],[String])
parse (l:ls) (minList,maxList,charList,passwordList) = let withoutSpace = split ' ' l
                                                           minMaxByteString = split '-' $ Prelude.head $ withoutSpace
                                                           minString = unpack $ Prelude.head minMaxByteString
                                                           min = Prelude.foldl (\x y-> 10*x+ord(y)-ord('0')) 0 minString
                                                           maxString = unpack $ Prelude.head $ Prelude.tail  minMaxByteString
                                                           max = Prelude.foldl (\x y-> 10*x+ord(y)-ord('0')) 0 maxString
                                                           searchChar = Prelude.head $ unpack $ Prelude.head $ Prelude.tail withoutSpace
                                                           password = unpack $ Prelude.head $ Prelude.tail $ Prelude.tail withoutSpace
                                                        in if Prelude.null (unpack l) == True then (minList,maxList,charList,passwordList) else parse ls ((min:minList),(max:maxList),(searchChar:charList),(password:passwordList))
parse [] ans = ans

solve:: ([Int],[Int],[Char],[String]) -> Int -> Int
solve ([],[],[],[]) ans = ans
solve ((min:mins),(max:maxs),(c:cs),(p:ps)) ans = let freq = Prelude.foldl (\y x -> if x==c then y+1 else y) 0 p
                                                  in if freq>=min && freq<=max then solve (mins,maxs,cs,ps) (ans+1) else solve (mins,maxs,cs,ps) ans
            


main:: IO()

main = do input <- System.IO.readFile "/home/fbi/Desktop/AdventOfCode/input.txt"
          let info@(minList,maxList,charList,passwordList) = parse (breakLines input) ([],[],[],[])
          print $ solve info 0 

          
