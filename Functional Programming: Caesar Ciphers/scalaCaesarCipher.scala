object Main {
	 def main(args: Array[String]) {
    	//Functional
      println("Caesar Cipher\n")
      val testStr1 = "IBM"
    	val testStr2 = "Hello World"
    	val testStr3 = "This is a test"
    	val testStr4 = "Hi my name is Robbie"
    	val testStr5 = "WandaVision"
    	val testStr6 = "Abed"
    	val testStr7 = "The Mandalorian"
    	val testStr8 = "Wow I learned Scala I think"
    	
    	val shiftAmount = -1
	    
	    var index = 0
	    var e = 0
	    
	    var testStrArr = Array(testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8)
	    
	    encrypt(testStrArr, shiftAmount, index, e)
	 }
	def encrypt(testStrArr:Array[String], shift:Int, index:Int, e:Int) : String = {
    	if(e == 0) {
        	testStrArr(index) = testStrArr(index).toUpperCase()
        	println("Original String: " + testStrArr(index))
        }
        var encryptedStr = testStrArr(index)
        var encryptedChrArr = encryptedStr.toCharArray()
        
        var letter = encryptedStr(e)
        var asciiVal = letter.toInt
        //println(letter) //check letters
        if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
            if(letter.toInt == 65 && shift < 0) { //If A and shift is negative, then loop around to Z
                encryptedChrArr(e) = (asciiVal + 26 + shift).toChar
            } else if(letter.toInt == 90 && shift > 0) { //If Z, and shift is positive, then loop around to A
                encryptedChrArr(e) = (asciiVal - 26 + shift).toChar
            } else { // Else perform a normal shift
                encryptedChrArr(e) = (asciiVal + shift).toChar
            }
        } else { //Other Characters
            //Other Characters should not be changed in Caesar Cipher
            encryptedChrArr(e) = letter
            //println(letter)
        }
    	  encryptedStr = encryptedChrArr.mkString("")
        testStrArr(index) = encryptedStr
        
        if(e == (encryptedStr.length - 1)) {
        	println("Encrypted String: " + encryptedStr)
        	var d = 0
        	var s = 0
        	val maxShiftAmount = 26
        	//Move to Decrypt
        	decrypt(testStrArr, shift, index, d)
        	//Move to Solve
        	println("Solve: ")
        	solve(testStrArr, encryptedStr, maxShiftAmount, index, s, maxShiftAmount)
        } else {
        	encrypt(testStrArr, shift, index, (e + 1))
        }
    }
    def decrypt(testStrArr:Array[String], shift:Int, index:Int, d:Int) : String = {
        var decryptedStr = testStrArr(index)
        var decryptedChrArr = decryptedStr.toCharArray()
        
        var letter = decryptedStr(d)
        var asciiVal = letter.toInt
        //println(letter) //check letters
        if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
            if(letter.toInt == 65 && -shift < 0) { //If A and shift is negative, then loop around to Z
                decryptedChrArr(d) = (asciiVal + 26 - shift).toChar
            } else if(letter.toInt == 90 && -shift > 0) { //If Z, and shift is positive, then loop around to A
                decryptedChrArr(d) = (asciiVal - 26 - shift).toChar
            } else { // Else perform a normal shift
                decryptedChrArr(d) = (asciiVal - shift).toChar
            }
        }
        else { //Other Characters
            //Other Characters should not be changed in Caesar Cipher
            decryptedChrArr(d) = letter
            //println(letter)
        }
    	  decryptedStr = decryptedChrArr.mkString("")
        testStrArr(index) = decryptedStr
        
        if(d == (decryptedStr.length - 1)) {
        	println("Decrypted String: " + decryptedStr)
        	return "Decrypt"
        } else {
        	decrypt(testStrArr, shift, index, (d + 1))
        }
    }
    def solve(testStrArr:Array[String], testStr:String, maxShift:Int, index:Int, x:Int, caesarNum:Int) : String = {
        var s = x
        var solveNum = caesarNum

        var solvedStr = testStr
        var solvedChrArr = solvedStr.toCharArray()
        if(solveNum == 26) {
          println("\tCaesar " + solveNum + ": " + solvedStr)
          solveNum -= 1
        }
        
        val shift = -1
        var tempMaxShift = 1
        
        var letter = solvedStr(s)
        var asciiVal = letter.toInt
        //println(letter) //check letters
        if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
            if(letter.toInt == 65 && shift < 0) { //If A and shift is negative, then loop around to Z
                solvedChrArr(s) = (asciiVal + 26 + shift).toChar
            } else if(letter.toInt == 90 && shift > 0) { //If Z, and shift is positive, then loop around to A
                solvedChrArr(s) = (asciiVal - 26 + shift).toChar
            } else { // Else perform a normal shift
                solvedChrArr(s) = (asciiVal + shift).toChar
            }
        }
        else { //Other Characters
            //Other Characters should not be changed in Caesar Cipher
            solvedChrArr(s) = letter
            //println(letter)
        }
    	solvedStr = solvedChrArr.mkString("")
        testStrArr(index) = solvedStr
        
        if((solveNum == 0) && (s == solvedStr.length - 1)) {
        	println("\tCaesar " + solveNum + ": " + solvedStr)
        	var e = 0
        	if(index != (testStrArr.length - 1)) {
        		println()
        		encrypt(testStrArr, shift, (index + 1), e)
        	} else {
        		return "Solve"
        	}
        } else {
        	if(s == (solvedStr.length - 1)) {
        		println("\tCaesar " + solveNum + ": " + solvedStr)
        		//tempMaxShift += 1
        		s = -1
        		solveNum -= 1
        		solve(testStrArr, solvedStr, (tempMaxShift + 1), index, (s + 1), solveNum)
        	} else {
        		solve(testStrArr, solvedStr, (tempMaxShift + 1), index, (s + 1), solveNum)
        	}
        }
    }
}
