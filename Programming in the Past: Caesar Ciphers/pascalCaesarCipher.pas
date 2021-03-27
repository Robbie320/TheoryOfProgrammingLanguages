program ideone;
(*define encrypt function*)
function encrypt(testStr: string; shift: integer): string;
var
	encryptedChrArr : array of char;
	letter : char;
	encryptedStr : string;
	x, e : integer;
BEGIN
    encryptedStr := '';
	SetLength(encryptedChrArr, length(testStr));
	
	for x:=0 to length(testStr)-1 do
	BEGIN
		encryptedChrArr[x] := testStr[x+1]; (*Add each character of string to a char array*)
		(*writeln(encryptedChrArr[x]);*)
	END; {for x}
	
	for e:=0 to length(encryptedChrArr)-1 do
	BEGIN
		letter := encryptedChrArr[e];
		(*writeln(letter);*) (*check letters*)
		if((Integer(letter) >= 65) and (Integer(letter) <= 90)) then BEGIN(*A to Z*)
			if((Integer(letter) = 65) and (shift < 0)) then BEGIN(*If A and shift is negative, then loop around to Z*)
				encryptedChrArr[e] := Chr(Integer(letter) + 26 + shift);
		    END else if((Integer(letter) = 90) and (shift > 0)) then BEGIN(*If Z, and shift is positive, then loop around to A*)
				encryptedChrArr[e] := Chr(Integer(letter) - 26 + shift);
		    END else BEGIN(*Perform a normal shift*)
				encryptedChrArr[e] := Chr(Integer(letter) + shift);
			END;
		END else BEGIN (*Other Characters*)
			encryptedChrArr[e] := letter;
		END; {if}
		(*turn character array into new string*)
		encryptedStr := encryptedStr + encryptedChrArr[e];
	END; {for e}
	encrypt := encryptedStr;
END; {function encrypt}

(*define decrypt function*)
function decrypt(testStr: string; shift: integer): string;
var
	decryptedChrArr : array of char;
	letter : char;
	decryptedStr : string;
	y, d : integer;
	
BEGIN
    decryptedStr := '';
	SetLength(decryptedChrArr, length(testStr));
	
	for y:=0 to length(testStr)-1 do
	BEGIN
		decryptedChrArr[y] := testStr[y+1]; (*Add each character of string to a char array*)
		(*writeln(decryptedChrArr[x]);*)
	END; {for y}
	
	for d:=0 to length(decryptedChrArr)-1 do
	BEGIN
		letter := decryptedChrArr[d];
		(*writeln(letter);*) (*check letters*)
		if((Integer(letter) >= 65) and (Integer(letter) <= 90)) then BEGIN(*A to Z*)
			if((Integer(letter) = 65) and (-shift < 0)) then BEGIN(*If A and shift is negative, then loop around to Z*)
				decryptedChrArr[d] := Chr(Integer(letter) + 26 - shift);
		    END else if((Integer(letter) = 90) and (-shift > 0)) then BEGIN(*If Z, and shift is positive, then loop around to A*)
				decryptedChrArr[d] := Chr(Integer(letter) - 26 - shift);
		    END else BEGIN(*Perform a normal shift*)
				decryptedChrArr[d] := Chr(Integer(letter) - shift);
			END;
		END else BEGIN (*Other Characters*)
			decryptedChrArr[d] := letter;
		END; {if}
		(*turn character array into new string*)
		decryptedStr := decryptedStr + decryptedChrArr[d];
	END; {for d}
	decrypt := decryptedStr;
END; {function decrpyt}


(*define solve function*)
function solve(testStr: string; maxShift: integer) : string;
var
	solvedChrArr : array of char;
	letter : char;
	solvedStr : string;
	shift : integer;
	tempNum : integer;
	n, j, s : integer;
	
BEGIN
	writeln(#9'Caesar 26: ' + testStr);
	solvedStr := '';
	shift := -1;
	SetLength(solvedChrArr, length(testStr));
	
	for n:=0 to length(testStr)-1 do
	BEGIN
		solvedChrArr[n] := testStr[n+1]; (*Add each character of string to a char array*)
		(*writeln(decryptedChrArr[x]);*)
	END; {for n}
	
	for j:=0 to maxShift-1 do
	BEGIN
		solvedStr := '';
		for s:=0 to length(solvedChrArr)-1 do
		BEGIN
			letter := solvedChrArr[s];
			(*writeln(letter);*) (*check letters*)
			if((Integer(letter) >= 65) and (Integer(letter) <= 90)) then BEGIN(*A to Z*)
				if((Integer(letter) = 65) and (shift < 0)) then BEGIN(*If A and shift is negative, then loop around to Z*)
					solvedChrArr[s] := Chr(Integer(letter) + 26 + shift);
			    END else if((Integer(letter) = 90) and (shift > 0)) then BEGIN(*If Z, and shift is positive, then loop around to A*)
					solvedChrArr[s] := Chr(Integer(letter) - 26 + shift);
			    END else BEGIN(*Perform a normal shift*)
					solvedChrArr[s] := Chr(Integer(letter) + shift);
				END;
			END else BEGIN (*Other Characters*)
				solvedChrArr[s] := letter;
			END; {if}
			(*turn character array into new string*)
			solvedStr := solvedStr + solvedChrArr[s];
		END; {for s}
		tempNum := maxShift-j-1;
		writeln(#9'Caesar ', tempNum,  ': ' + solvedStr);
	END; {for j}
	solve := solvedStr;
END; {function solve}

var
	testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8 : string;
	shiftAmount, maxShiftAmount : integer;
	testStrArr : array[0..7] of string;
	
	tempStr, encryptedTempStr, decryptedTempStr, solvedTempStr : string;
	i : integer;
	
BEGIN
	writeln('Caesar Cipher');
	writeln();
	testStr1 := 'IBM';
	testStr2 := 'Hello World';
	testStr3 := 'This is a test';
	testStr4 := 'Hi my name is Robbie';
	testStr5 := 'WandaVision';
	testStr6 := 'Abed';
	testStr7 := 'The Mandalorian';
	testStr8 := 'Wow I learned Pascal I think';
	
	shiftAmount := -1;
	maxShiftAmount := 26;
	
	testStrArr[0] := testStr1; testStrArr[1] := testStr2;
	testStrArr[2] := testStr3; testStrArr[3] := testStr4;
	testStrArr[4] := testStr5; testStrArr[5] := testStr6;
	testStrArr[6] := testStr7; testStrArr[7] := testStr8;
	
	for i:=0 to length(testStrArr)-1 do
	BEGIN
		tempStr := UpCase(testStrArr[i]); (*Change all letters to uppercase*)
		writeln('Original String: ' + tempStr);
		
		encryptedTempStr := encrypt(tempStr, shiftAmount);
		writeln('Encrypted String: ' + encryptedTempStr);
		
		decryptedTempStr := decrypt(encryptedTempStr, shiftAmount);
		writeln('Decrypted String: ' + decryptedTempStr);
		
		writeln('Solve: ');
		solvedTempStr := solve(encryptedTempStr, maxShiftAmount);
		writeln();
	END; {for i}
END. {program ideone}
