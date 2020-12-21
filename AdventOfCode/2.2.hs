import System.IO
import Data.ByteString.Char8
import Data.Char
xor:: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False

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
solve ((min:mins),(max:maxs),(c:cs),(p:ps)) ans = let f = \(poz,rez) x -> if ((poz==min) || (poz==max)) && x==c then (poz+1,xor rez True) else (poz+1,xor rez False)
                                                      per = Prelude.foldl f (1,False) p
                                                      in if (snd per) == True then solve (mins,maxs,cs,ps) (ans+1) else solve (mins,maxs,cs,ps) ans
solve ([],[],[],[]) ans = ans 


main:: IO()
main = do input <- System.IO.readFile "/home/fbi/Desktop/AdventOfCode/input.txt"
          let info@(minList,maxList,charList,passwordList) = parse (breakLines input) ([],[],[],[])
          print $ solve info 0 
          
