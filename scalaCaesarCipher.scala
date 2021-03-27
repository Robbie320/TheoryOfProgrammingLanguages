object Main {
    def main(args: Array[String]) {
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
	    val maxShiftAmount = 26
	    
	    var testStrArr = Array(testStr1, testStr2, testStr3, testStr4, testStr5, testStr6, testStr7, testStr8)
	    
	    for(i <- testStrArr) {
	        var tempStr = i.toUpperCase()
	        println("Original String: " + tempStr)
	        
	        var encryptedTempStr = encrypt(tempStr, shiftAmount)
	        println("Encrypted String: " + encryptedTempStr)
	        
	        var decryptedTempStr = decrypt(encryptedTempStr, shiftAmount)
	    	println("Decrypted String: " + decryptedTempStr)
	        
	        println("Solve: ")
	        solve(encryptedTempStr, maxShiftAmount)
	        println()
	    }
    }
    def encrypt(testStr:String, shift:Int) : String /*Array[Char]*/= {
        //var encryptedStr = ""
        var encryptedChrArr = testStr.toCharArray()
        var e = 0
        
        for(e <- 0 until testStr.length()) {
            var letter = encryptedChrArr(e) //testStr.charAt(e)
            //println(letter) //check letters
            if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
                if(letter.toInt == 65 && shift < 0) { //If A and shift is negative, then loop around to Z
                    encryptedChrArr(e) = (letter.toInt + 26 + shift).toChar
                } else if(letter.toInt == 90 && shift > 0) { //If Z, and shift is positive, then loop around to A
                    encryptedChrArr(e) = (letter.toInt - 26 + shift).toChar
                } else { // Else perform a normal shift
                    encryptedChrArr(e) = (letter.toInt + shift).toChar
                    //encryptedStr.concat(letter.toString)
                    //encryptedChrArr(e) = (testStr.charAt(e).toInt + shift)
                    //println(letter)
                }
            }
            else { //Other Characters
                //Other Characters should not be changed in Caesar Cipher
                encryptedChrArr(e) = letter
                //println(letter)
            }
        }
        var encryptedStr = encryptedChrArr.mkString("")
        return encryptedStr
    }
    def decrypt(testStr:String, shift:Int) : String = {
        var decryptedChrArr = testStr.toCharArray()
        var d = 0
        
        for(d <- 0 until testStr.length()) {
            var letter = decryptedChrArr(d)
            //shift = -shift//Switch sign of shift
            //println(letter)
            if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
                if(letter.toInt == 65 && -shift < 0) { //If A and shift is negative, then loop around to Z
                    decryptedChrArr(d) = (letter.toInt + 26 - shift).toChar
                } else if(letter.toInt == 90 && -shift > 0) { //If Z, and shift is positive, then loop around to A
                    decryptedChrArr(d) = (letter.toInt - 26 - shift).toChar
                } else { // Else perform a normal shift
                    decryptedChrArr(d) = (letter.toInt - shift).toChar
                    //println(letter)
                }
            }
            else { //Other Characters
                //Other Characters should not be changed in Caesar Cipher
                decryptedChrArr(d) = letter
                //println(letter)
            }
        }
        var decryptedStr = decryptedChrArr.mkString("")
        return decryptedStr
    }
    def solve(testStr:String, maxShift:Int) {
        var solvedChrArr = testStr.toCharArray()
        val shift = -1
        var j = 0
        var s = 0
        
        println("\tCaesar " + (26).toString + ": " + testStr)
        for(j <- 0 until maxShift) {
	        for(s <- 0 until testStr.length()) {
	            var letter = solvedChrArr(s)
	            //println(letter)
	            if(letter.toInt >= 65 && letter.toInt <= 90) { //A to Z
	                if(letter.toInt == 65 && shift < 0) { //If A and shift is negative, then loop around to Z
	                    solvedChrArr(s) = (letter.toInt + 26 + shift).toChar
	                } else if(letter.toInt == 90 && shift > 0) { //If Z, and shift is positive, then loop around to A
	                    solvedChrArr(s) = (letter.toInt - 26 + shift).toChar
	                } else { // Else perform a normal shift
	                    solvedChrArr(s) = (letter.toInt + shift).toChar
	                    //println(letter)
	                }
	            }
	            else { //Other Characters
	                solvedChrArr(s) = letter
	                //println(letter)
	            }
	        }
	        var solvedStr = solvedChrArr.mkString("")
	        println("\tCaesar " + (maxShift-j-1).toString + ": " + solvedStr)
    	}
    }
}