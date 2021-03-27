Imports System
Module Test
	Function encrypt(testStr as string, shift as integer) as string
		DIM encryptedStr as string
		DIM e as integer
		
		for e = 0 to len(testStr)-1
			DIM letter as char = testStr(e)
			if((Asc(letter) >= 65) and (Asc(letter) <= 90)) then
				if((Asc(letter) = 65) and (shift < 0)) then
					encryptedStr += Chr(Asc(letter) + 26 + shift)
				else if((Asc(letter) = 90) and (shift > 0)) then
					encryptedStr += Chr(Asc(letter) - 26 + shift)
				else
					encryptedStr += Chr(Asc(letter) + shift)
				End if
			Else
				encryptedStr += letter
			End if
		Next
		encrypt = encryptedStr
	End Function
	
	Function decrypt(testStr as string, shift as integer) as string
		DIM decryptedStr as string
		DIM d as integer
		
		for d = 0 to len(testStr)-1
			DIM letter as char = testStr(d)
			if((Asc(letter) >= 65) and (Asc(letter) <= 90)) then
				if((Asc(letter) = 65) and (-shift < 0)) then
					decryptedStr += Chr(Asc(letter) + 26 - shift)
				else if((Asc(letter) = 90) and (-shift > 0)) then
					decryptedStr += Chr(Asc(letter) - 26 - shift)
				else
					decryptedStr += Chr(Asc(letter) - shift)
				End if
			Else
				decryptedStr += letter
			End if
		Next
		decrypt = decryptedStr
	End Function
	
	Function solve(testStr as string, maxShift as integer) as string
		DIM solvedChrArr() as char = testStr.ToCharArray
		DIM solvedStr as string
		DIM shift as integer = -1
		DIM j as integer
		DIM s as integer
		
		Console.WriteLine(vbTab & "Caesar 26: " + testStr)
		for j = 0 to maxShift-1
			for s = 0 to len(testStr)-1
				DIM letter as char = solvedChrArr(s)
				if((Asc(letter) >= 65) and (Asc(letter) <= 90)) then
					if((Asc(letter) = 65) and (shift < 0)) then
						solvedChrArr(s) = Chr(Asc(letter) + 26 + shift)
					else if((Asc(letter) = 90) and (shift > 0)) then
						solvedChrArr(s) = Chr(Asc(letter) - 26 + shift)
					else
						solvedChrArr(s) = Chr(Asc(letter) + shift)
					End if
				Else
					solvedChrArr(s) = letter
				End if
			Next
			solvedStr = New String(solvedChrArr)
			DIM tempNum as integer = maxShift-j-1
			Console.WriteLine(vbTab & "Caesar " + tempNum.toString() + ": " + solvedStr)
		Next
		solve = solvedStr
	End Function
	
	Sub Main()
		' your code goes here
		Console.WriteLine("Caesar Cipher" & vbLf)
		DIM testStr1 as string = "IBM"
    	DIM testStr2 as string = "Hello World"
    	DIM testStr3 as string = "This is a test"
    	DIM testStr4 as string = "Hi my name is Robbie"
    	DIM testStr5 as string = "WandaVision"
    	DIM testStr6 as string = "Abed"
    	DIM testStr7 as string = "The Mandalorian"
    	DIM testStr8 as string = "Wow I learned BASIC I think"
    	
    	DIM shiftAmount as integer = -1
    	DIM maxShiftAmount as integer = 26
    	
    	DIM testStrArr = New String() {testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8}
    	
    	DIM i as integer
    	for i = 0 to (testStrArr.length - 1)
    		'Console.WriteLine(testStrArr(i))
    		DIM tempStr as string = UCase(testStrArr(i))
    		Console.WriteLine("Original String: " + tempStr)
    		
    		DIM encryptedTempStr as string = encrypt(tempStr, shiftAmount)
    		Console.WriteLine("Encrypted String: " + encryptedTempStr)
    		
    		DIM decryptedTempStr as string = decrypt(encryptedTempStr, shiftAmount)
    		Console.WriteLine("Decrypted String: " + decryptedTempStr)
    		
    		Console.WriteLine("Solve: ")
    		solve(encryptedTempStr, maxShiftAmount)
    		Console.WriteLine()
    	next
	End Sub
End Module
