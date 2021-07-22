open System

//(*Negative Shift*)//
let negShift(character, shiftAmount) =
  let asciiVal = int character
  //if char is A or a
  if (character = 'A') || (character = 'a') then
    char (asciiVal + shiftAmount + 26)
  //if B >= character <= Z or b >= character < Z
  elif (List.contains character ['B'..'Z']) || (List.contains character ['b'..'z']) then
    char (asciiVal + shiftAmount)
  else character
//(*Positive Shift*)//
let posShift(character, shiftAmount) =
  let asciiVal = int character
  //if char is Z or z
  if (character = 'Z') || (character = 'z') then
    char (asciiVal + shiftAmount - 26)
  //if A >= character <= Y or a >= character < y
  elif (List.contains character ['A'..'Y']) || (List.contains character ['a'..'y']) then
    char (asciiVal + shiftAmount)
  else character

//(*Decide which *)//
let pickShift(testStr, shiftAmount) =
  if (shiftAmount < 0) then 
    String.map(fun character -> negShift(character, shiftAmount)) testStr
  elif (shiftAmount > 0) then
    String.map(fun character -> posShift(character, shiftAmount)) testStr
  else testStr

//(*ENCRYPT*)//
let encrypt(testStrList, shiftAmount) =
  List.map(fun testStr -> pickShift(testStr, shiftAmount)) testStrList
  (*match testStrList with
  | [] -> []
  | head :: tail ->
    pickShift(head, shiftAmount)
    encrypt(tail, shiftAmount)*)
//(*DECRYPT*)//
let decrypt(encryptedList, shiftAmount) =
  List.map(fun encryptedStr -> pickShift(encryptedStr, shiftAmount)) encryptedList
//(*SOLVE*)//
let rec solve(solvedList, shiftAmount, maxShift, solveNum) =
  let newList = List.map(fun encryptedStr -> pickShift(encryptedStr, shiftAmount)) solvedList
  printfn "%A" solvedList
  if solveNum <> maxShift then
    solve(newList, shiftAmount, maxShift, (solveNum + 1))
  else newList

//(*MAIN*)//
let main() =
  let testStr1 = "IBM"
  let testStr2 = "Hello World"
  let testStr3 = "This is a test"
  let testStr4 = "Hi my name is Robbie"
  let testStr5 = "WandaVision"
  let testStr6 = "Abed"
  let testStr7 = "The Mandalorian"
  let testStr8 = "Wow I learned F# I think"
  
  let testStrList = [testStr1; testStr2; testStr3; testStr4; testStr5; testStr6; testStr7; testStr8;]
  
  let shiftAmount = (-1)
  let maxShift = 26
  let solveNum = 0
  
  printfn "Original Strings:"
  printfn "%A" testStrList
  
  printfn "\nEncrypted Strings:"
  let encryptedList = encrypt(testStrList, shiftAmount)
  printfn "%A" encryptedList
  
  printfn "\nDecrypted Strings:"
  let decryptedList = decrypt(encryptedList, (-shiftAmount))
  printfn "%A" decryptedList
  
  printfn "\nSolve:"
  let mutable solvedList = encryptedList
  solvedList <- solve(solvedList, shiftAmount, maxShift, solveNum)

main()
