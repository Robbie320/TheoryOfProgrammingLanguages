!Encrypt
function encrypt(testStr, shift) result(encryptedStr)
	implicit none
	
	!local variables
	character(len = 30), intent(in) :: testStr
	integer, intent(in) :: shift
	
	!character, dimension(30) :: encryptedChrArr
	character :: letter
	integer :: asciiVal, e
	
	character(len = 30) :: encryptedStr
	
	encryptedStr = trim(testStr)
	eloop: do e = 1, len(encryptedStr)
	    !if character from A to Z
		letter = encryptedStr(e:e)
		asciiVal = ichar(letter)
		if((asciiVal >= 65) .and. (asciiVal <= 90)) then
			if((asciiVal == 65) .and. (shift < 0)) then
				encryptedStr(e:e) = char(asciiVal + 26 + shift)
			else if((asciiVal == 90 ) .and. (shift > 0)) then
				encryptedStr(e:e) = char(asciiVal - 26 + shift)
			else
				encryptedStr(e:e) = char(asciiVal + shift)
			end if
		!if character from a to z
		else if((asciiVal >= 97) .and. (asciiVal <= 122)) then
			if((asciiVal == 97) .and. (shift < 0)) then
				encryptedStr(e:e) = char(asciiVal + 26 + shift)
			else if((asciiVal == 122) .and. (shift > 0)) then
				encryptedStr(e:e) = char(asciiVal - 26 + shift)
			else
				encryptedStr(e:e) = char(asciiVal + shift)
			end if
		else
		    encryptedStr(e:e) = letter
		end if
	end do eloop
end function encrypt

!Decrypt
function decrypt(testStr, shift) result(decryptedStr)
	implicit none
	
	!local variables
	character(len = 30), intent(in) :: testStr
	integer, intent(in) :: shift
	
	character :: letter
	integer :: asciiVal, d
	
	character(len = 30) :: decryptedStr
	
	decryptedStr = trim(testStr)
	dloop: do d = 1, len(decryptedStr)
	    !if character from A to Z
		letter = decryptedStr(d:d)
		asciiVal = ichar(letter)
		if((asciiVal >= 65) .and. (asciiVal <= 90)) then
			if((asciiVal == 65) .and. (-shift < 0)) then
				decryptedStr(d:d) = char(asciiVal + 26 - shift)
			else if((asciiVal == 90 ) .and. (-shift > 0)) then
				decryptedStr(d:d) = char(asciiVal - 26 - shift)
			else
				decryptedStr(d:d) = char(asciiVal - shift)
			end if
		!if character from a to z
		else if((asciiVal >= 97) .and. (asciiVal <= 122)) then
			if((asciiVal == 97) .and. (-shift < 0)) then
				decryptedStr(d:d) = char(asciiVal + 26 - shift)
			else if((asciiVal == 122) .and. (-shift > 0)) then
				decryptedStr(d:d) = char(asciiVal - 26 - shift)
			else
				decryptedStr(d:d) = char(asciiVal - shift)
			end if
		else
		    decryptedStr(d:d) = letter
		end if
	end do dloop
end function decrypt

!Solve
function solve(testStr, maxShift) result(solvedStr)
	implicit none
	
	!local variables
	character(len = 30), intent(in) :: testStr
	integer, intent(in) :: maxShift
	
	!character, dimension(30) :: solvedChrArr
	character :: letter
	integer :: shift, caesarNum
	integer :: asciiVal, j, s
	
	character(len = 30) :: solvedStr
	
	shift = -1
	solvedStr = trim(testStr)
	PRINT *, char(9), 'Caesar 26: ', solvedStr
	jloop: do j = 0, maxShift-1
    	sloop: do s = 1, len(solvedStr)
    	    !if character from A to Z
    		letter = solvedStr(s:s)
    		asciiVal = ichar(letter)
    		if((asciiVal >= 65) .and. (asciiVal <= 90)) then
    			if((asciiVal == 65) .and. (shift < 0)) then
    				solvedStr(s:s) = char(asciiVal + 26 + shift)
    			else if((asciiVal == 90 ) .and. (shift > 0)) then
    				solvedStr(s:s) = char(asciiVal - 26 + shift)
    			else
    				solvedStr(s:s) = char(asciiVal + shift)
    			end if
    		!if character from a to z
    		else if((asciiVal >= 97) .and. (asciiVal <= 122)) then
    			if((asciiVal == 97) .and. (shift < 0)) then
    				solvedStr(s:s) = char(asciiVal + 26 + shift)
    			else if((asciiVal == 122) .and. (shift > 0)) then
    				solvedStr(s:s) = char(asciiVal - 26 + shift)
    			else
    				solvedStr(s:s) = char(asciiVal + shift)
    			end if
    		else
    		    solvedStr(s:s) = letter
    		end if
    	end do sloop
    	caesarNum = maxShift-j-1
    	print *, char(9), 'Caesar ', caesarNum, ': ', solvedStr
    end do jloop
end function solve

program MAIN
!makes sure that variables "i,j,k,l,m,n" are not default to integer
	implicit none
	
!Declare variables 
	character(len = 30) :: encrypt !return type
	character(len = 30) :: decrypt !return type
	character(len = 30) :: solve   !return type
	
	character(len = 30) :: testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8
	character(len = 30), dimension(10) :: testStrArr(8) !one dimensional array
	character(len = 30) :: tempStr, encryptedTempStr, decryptedTempStr, solvedTempStr
	
	integer :: shiftAmount
	integer :: maxShiftAmount
	integer :: i
	
!Initialize variables
	testStr1 = 'IBM'
	testStr2 = 'Hello World'
	testStr3 = 'This is a test'
	testStr4 = 'Hi my name is Robbie'
	testStr5 = 'WandaVision'
	testStr6 = 'Abed'
	testStr7 = 'The Mandalorian'
	testStr8 = 'Wow I learned Fortran I think'
	
	shiftAmount = -1
	maxShiftAmount = 26
	
	
	testStrArr = (/ testStr1,testStr2,testStr3,testStr4,testStr5,testStr6,testStr7,testStr8 /)
	
	PRINT *, 'Caesar Cipher'
	PRINT *, ''
	
!Do loop to encrypt, decrypt, and solve
	iloop: do i = 1, SIZE(testStrArr)
		tempStr = (testStrArr(i))
		PRINT *, 'Original String: ', tempStr
		
		encryptedTempStr = encrypt(tempStr, shiftAmount)
		PRINT *, 'Encrypted String: ', encryptedTempStr
		decryptedTempStr = decrypt(encryptedTempStr, shiftAmount)
		PRINT *, 'Decrpyted String: ', decryptedTempStr
		
		PRINT *, 'Solve: '
		solvedTempStr = solve(encryptedTempStr, maxShiftAmount)
		PRINT *, ''
	end do iloop
end program MAIN
