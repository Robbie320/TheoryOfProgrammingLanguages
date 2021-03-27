$ SET SOURCEFORMAT "FREE"

IDENTIFICATION DIVISION.
PROGRAM-ID. IDEONE.
AUTHOR. Robert Perrone.
DATE-WRITTEN. March 22nd 2021
*>ENVIRONMENT DIVISION.
	
DATA DIVISION.
WORKING-STORAGE SECTION.
*> Original String Table
01 testStrTable.*> PIC X(30).
	02 testStr1 PIC X(30) VALUE "IBM".
	02 testStr2 PIC X(30) VALUE "Hello World".
	02 testStr3 PIC X(30) VALUE "This is a test".
	02 testStr4 PIC X(30) VALUE "Hi my name is Robbie".
	02 testStr5 PIC X(30) VALUE "WandaVision".
	02 testStr6 PIC X(30) VALUE "Abed".
	02 testStr7 PIC X(30) VALUE "The Mandalorian".
	02 testStr8 PIC X(30) VALUE "Wow I learned COBOL I think".
01 FILLER REDEFINES testStrTable.
	02 testStrTb OCCURS 8 INDEXED BY I.
		03 testStrTb OCCURS 1 TIMES.
			04 testStr PIC X(30).

*> Encrypted String Table
01 eTestStrTable PIC X(30).
	02 eTestStr1 PIC X(30) VALUE "".
	02 eTestStr2 PIC X(30) VALUE "".
	02 eTestStr3 PIC X(30) VALUE "".
	02 eTestStr4 PIC X(30) VALUE "".
	02 eTestStr5 PIC X(30) VALUE "".
	02 eTestStr6 PIC X(30) VALUE "".
	02 eTestStr7 PIC X(30) VALUE "".
	02 eTestStr8 PIC X(30) VALUE "".
01 FILLER REDEFINES eTestStrTable.
	02 eTestStrTb OCCURS 8 TIMES INDEXED BY J.
		03 eTestStrTb OCCURS 1 TIMES.
			04 eTestStr PIC X(30).

01 shiftAmount PIC S9(4) VALUE -1.
01 maxShiftAmount PIC 9(2) VALUE 26.

01 tempStr PIC X(30) VALUE "".
01 encryptedStr PIC X(30) VALUE "".
01 decryptedStr PIC X(30) VALUE "".
01 solvedStr PIC X(30) VALUE "".

*>01 I PIC 9(1) VALUE 1.
*>01 J PIC 9(1) VALUE 1.

PROCEDURE DIVISION.

*>	your code goes here
	DISPLAY "Caesar Cipher ".
	DISPLAY "".
	
	SET I TO 1.
	SET J TO 1.
	begin.
		PERFORM iForLoop VARYING I FROM 1 BY 1 UNTIL I>8.
		STOP RUN.

	iForLoop.
		MOVE Function UPPER-CASE(testStr(I)) TO tempStr.
		DISPLAY "Original String: " tempStr.
		
		CALL "ENCRYPT" USING tempStr, shiftAmount, encryptedStr.
		DISPLAY "Encrypted String: "  encryptedStr.
		
		CALL "DECRYPT" USING encryptedStr, shiftAmount, decryptedStr.
		DISPLAY "Decrypted String: " decryptedStr.
		
		DISPLAY "Solve:".
		CALL "SOLVE" USING encryptedStr, maxShiftAmount, solvedStr.
		DISPLAY "".
		*>MOVE Function solve(encryptedTempStr, maxShiftAmount) TO decryptedTempStr.
END PROGRAM IDEONE.

*>Encrypt Subprogram
IDENTIFICATION DIVISION.
PROGRAM-ID. ENCRYPT.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 letter PIC X(1).
01 newLetter PIC X(1).
01 asciiVal PIC 9(2).
01 newAsciiVal PIC 9(2).

01 E PIC 9(2) VALUE 1.
01 len PIC 9(3) VALUE 0.

LINKAGE SECTION.
01 tempStr PIC X(30).
01 shiftAmount PIC S9(4).
01 encryptedStr PIC X(30).

PROCEDURE DIVISION USING tempStr, shiftAmount, encryptedStr.
	*>MOVE "returned" TO encryptedStr.
	*>DISPLAY "ENCRYPT".
	begin.
		SET shiftAmount TO -1.
		MOVE Function LENGTH(tempStr) TO len.
		*>DISPLAY len.
		PERFORM eForLoop VARYING E FROM 1 BY 1 UNTIL E > len.
			EXIT PROGRAM.
	eForLoop.
		MOVE tempStr(E:E) TO letter.
		*>DISPLAY letter.
		MOVE Function ORD(letter) TO asciiVal.
		*>COMPUTE asciiVal = asciiVal - 1.
		*>DISPLAY asciiVal.
		
		*> If character from A to Z
		IF (asciiVal >= 66) AND (asciiVal <= 91) THEN
			*>If character is A and shift is negative, wrap around
			IF (asciiVal = 66) AND (shiftAmount < 0) THEN
				COMPUTE newAsciiVal = asciiVal + 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO encryptedStr(E:E).
			
			*>If character is Z and shift is positive, wrap around
			IF ((asciiVal = 91) AND (shiftAmount > 0)) THEN
				COMPUTE newAsciiVal = asciiVal - 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO encryptedStr(E:E).
			*>ELSE
			IF NOT ((asciiVal = 66) AND (shiftAmount < 0)) AND NOT ((asciiVal = 91) AND (shiftAmount > 0)) THEN
				*>DISPLAY letter.
				*>DISPLAY asciiVal.
				COMPUTE newAsciiVal = asciiVal + shiftAmount.
				*>DISPLAY shiftAmount.
				*>DISPLAY newAsciiVal.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO encryptedStr(E:E).
				*>DISPLAY encryptedStr(E:E).
			
		IF NOT ((asciiVal >= 66) AND (asciiVal <= 91)) THEN
			*>If character is space or other
			MOVE letter TO encryptedStr(E:E).
		
END PROGRAM ENCRYPT.

*>Decrypt Subprogram
IDENTIFICATION DIVISION.
PROGRAM-ID. DECRYPT.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 letter PIC X(1).
01 newLetter PIC X(1).
01 asciiVal PIC 9(2).
01 newAsciiVal PIC 9(2).

01 D PIC 9(2) VALUE 1.
01 len PIC 9(3) VALUE 0.

LINKAGE SECTION.
01 encryptedStr PIC X(30).
01 shiftAmount PIC S9(4).
01 decryptedStr PIC X(30).

PROCEDURE DIVISION USING encryptedStr, shiftAmount, decryptedStr.
	*>MOVE "returned" TO encryptedStr.
	*>DISPLAY "DECRYPT".
	begin.
		MOVE Function LENGTH(encryptedStr) TO len.
		*>DISPLAY len.
		MULTIPLY shiftAmount BY -1 GIVING shiftAmount.
		PERFORM dForLoop VARYING D FROM 1 BY 1 UNTIL D > len.
			EXIT PROGRAM.
	dForLoop.
		MOVE encryptedStr(D:D) TO letter.
		*>DISPLAY letter.
		MOVE Function ORD(letter) TO asciiVal.
		*>COMPUTE asciiVal = asciiVal - 1.
		*>DISPLAY asciiVal.
		*> If character from A to Z
		
		*>Make shift amount opposite sign
		
		
		IF (asciiVal) >= 66 AND (asciiVal <= 91) THEN
			*>If character is A and shift is negative, wrap around
			IF (asciiVal = 66) AND (shiftAmount < 0) THEN
				COMPUTE newAsciiVal = asciiVal + 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO decryptedStr(D:D).
			
			*>If character is Z and shift is positive, wrap around
			IF (asciiVal = 91) AND (shiftAmount > 0) THEN
				COMPUTE newAsciiVal = asciiVal - 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO decryptedStr(D:D).
			*>ELSE
			IF NOT ((asciiVal = 66) AND (shiftAmount < 0)) AND NOT ((asciiVal = 91) AND (shiftAmount > 0)) THEN
				*>DISPLAY letter.
				*>DISPLAY asciiVal.
				COMPUTE newAsciiVal = asciiVal + shiftAmount.
				*>DISPLAY shiftAmount.
				*>DISPLAY newAsciiVal.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO decryptedStr(D:D).
				*>DISPLAY encryptedStr(D:D).
			
		IF NOT ((asciiVal >= 66) AND (asciiVal <= 91)) THEN
			*>If character is space or other
			MOVE letter TO decryptedStr(D:D).
		
END PROGRAM DECRYPT.

*>Solve Subprogram
IDENTIFICATION DIVISION.
PROGRAM-ID. SOLVE.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 letter PIC X(1).
01 newLetter PIC X(1).
01 asciiVal PIC 9(2).
01 newAsciiVal PIC 9(2).
01 caesarNum PIC 9(2).
01 shiftAmount PIC S9(4) VALUE -1.

01 J PIC 9(2) VALUE 1.
01 S PIC 9(2) VALUE 1.
01 len PIC 9(3) VALUE 0.

LINKAGE SECTION.
01 encryptedStr PIC X(30).
01 maxShiftAmount PIC 9(2).
01 solvedStr PIC X(30).

PROCEDURE DIVISION USING encryptedStr, maxShiftAmount, solvedStr.
	*>MOVE "returned" TO solvedStr.
	*>DISPLAY "SOLVE".
	begin.
		MOVE Function LENGTH(solvedStr) TO len.
		SET shiftAmount TO -1.
		MOVE encryptedStr TO solvedStr.
		DISPLAY "Caesar 26: " solvedStr.
		PERFORM jForLoop VARYING J FROM 1 BY 1 UNTIL J > maxShiftAmount.
			EXIT PROGRAM.
	
	jForLoop.
		PERFORM sForLoop VARYING S FROM 1 BY 1 UNTIL S > len.
		COMPUTE caesarNum = maxShiftAmount - j.
		DISPLAY "Caesar " caesarNum ": " solvedStr.
		
	sForLoop.
		MOVE solvedStr(S:S) TO letter.
		*>DISPLAY letter.
		MOVE Function ORD(letter) TO asciiVal.
		*>COMPUTE asciiVal = asciiVal - 1.
		*>DISPLAY asciiVal.
		*> If character from A to Z
		IF (asciiVal >= 66) AND (asciiVal <= 91) THEN
			*>If character is A and shift is negative, wrap around
			IF (asciiVal = 66) AND (shiftAmount < 0) THEN
				COMPUTE newAsciiVal = asciiVal + 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO solvedStr(S:S).
			*>If character is Z and shift is positive, wrap around
			IF (asciiVal = 91) AND (shiftAmount > 0) THEN
				COMPUTE newAsciiVal = asciiVal - 26 + shiftAmount.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO solvedStr(S:S).
			*>ELSE
			IF NOT ((asciiVal = 66) AND (shiftAmount < 0)) AND NOT ((asciiVal = 91) AND (shiftAmount > 0)) THEN
				*>DISPLAY letter.
				*>DISPLAY asciiVal.
				COMPUTE newAsciiVal = asciiVal + shiftAmount.
				*>DISPLAY shiftAmount.
				*>DISPLAY newAsciiVal.
				MOVE Function CHAR(newAsciiVal) TO newLetter.
				MOVE newLetter TO solvedStr(S:S).
		IF NOT ((asciiVal >= 66) AND (asciiVal <= 91)) THEN
			*>If character is space or other
			MOVE letter TO solvedStr(S:S).
		
END PROGRAM SOLVE.
