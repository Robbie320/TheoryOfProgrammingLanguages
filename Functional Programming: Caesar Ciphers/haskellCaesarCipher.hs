import Data.Char
import System.IO
import Control.Monad

shiftAmount = (-1) -- Global Shift (Constant)

--Alternate Solution, issue was that I could not print out
--repeatNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
--repeatNtimes 1 f x = f x
--repeatNtimes maxShift f x = f (repeatNtimes (maxShift-1) f x) -> print(f x)

--Encrypt
encrypt :: [String] -> [String]
encrypt [] = [] --return blank list if given blank list
encrypt(x:xs) = enShift x : encrypt xs --Own map function to "enShift" each item in the list
--Decrypt
decrypt :: [String] -> [String]
decrypt [] = [] --return blank list if given blank list
decrypt(x:xs) = deShift x : decrypt xs --Own map function to "deShift" each item in the list
--Solve
solve :: [String] -> [String]
solve [] = []
solve(x:xs) = enShift x : solve xs

--Encrypt Shift to decide based on +shiftAmount
enShift :: [Char] -> [Char]
enShift testStr
  | shiftAmount < 0 = (map negEnShift testStr)
  | shiftAmount > 0 = (map posEnShift testStr) 
  | shiftAmount == 0 = testStr
--Decrypt Shift to decide based on -shiftAmount
deShift :: [Char] -> [Char]
deShift testStr
  | (-shiftAmount) < 0 = (map negDeShift testStr) 
  | (-shiftAmount) > 0 = (map posDeShift testStr)
  | (-shiftAmount) == 0 = testStr

--Negative Encrypt Shift (with +shiftAmount)
negEnShift :: Char -> Char
negEnShift character
  | character == 'A' = chr ((ord character) + shiftAmount + 26)
  | character == 'a' = chr ((ord character) + shiftAmount + 26)
  | character `elem` ['B'..'Z'] = chr ((ord character) + shiftAmount)
  | character `elem` ['b'..'z'] = chr ((ord character) + shiftAmount)
  | otherwise = character
--Positive Encrypt Shift (with +shiftAmount)
posEnShift :: Char -> Char
posEnShift character
  | character == 'Z' = chr ((ord character) + shiftAmount - 26)
  | character == 'z' = chr ((ord character) + shiftAmount - 26)
  | character `elem` ['A'..'Y'] = chr ((ord character) + shiftAmount)
  | character `elem` ['a'..'y'] = chr ((ord character) + shiftAmount)
  | otherwise = character

--Negative Decrypt Shift (with -shiftAmount)
negDeShift :: Char -> Char
negDeShift character
  | character == 'A' = chr ((ord character) - shiftAmount + 26)
  | character == 'a' = chr ((ord character) - shiftAmount + 26)
  | character `elem` ['B'..'Z'] = chr ((ord character) - shiftAmount)
  | character `elem` ['b'..'z'] = chr ((ord character) - shiftAmount)
  | otherwise = character
--Positive Decrypt Shift (with -shiftAmount)
posDeShift :: Char -> Char
posDeShift character
  | character == 'Z' = chr ((ord character) - shiftAmount - 26)
  | character == 'z' = chr ((ord character) - shiftAmount - 26)
  | character `elem` ['A'..'Y'] = chr ((ord character) - shiftAmount)
  | character `elem` ['a'..'y'] = chr ((ord character) - shiftAmount)
  | otherwise = character

main = do
  let testStr1 = "IBM"
  let testStr2 = "Hello World"
  let testStr3 = "This is a test"
  let testStr4 = "Hi my name is Robbie"
  let testStr5 = "WandaVision"
  let testStr6 = "Abed"
  let testStr7 = "The Mandalorian"
  let testStr8 = "Wow I learned Haskell I think"
  
  let testStrList = [testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8]
  
  let maxShift = 27 --includes "Caesar 26:" to "Caesar 0:"
  
  -- FUNCTION CALLS --
  putStrLn "Original Strings: "
  mapM putStrLn testStrList
  putStrLn ""
  
  putStrLn "Encrypted Strings: "
  let encryptedList = encrypt testStrList
  mapM putStrLn encryptedList
  putStrLn ""
  
  putStrLn "Decrypted Strings: "
  let decryptedList = decrypt encryptedList
  mapM putStrLn decryptedList
  putStrLn ""
  
  putStrLn "Solve: "
  let solvedList = encryptedList
  --putStr "Caesar 26: "
  --print(solvedList)
  
  mapM print(take maxShift $ iterate (map (solve)) [solvedList])
  --print(repeatNtimes maxShift solve solvedList)
  --print(solve encryptedList)
