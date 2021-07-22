(defvar testStr1 "IBM")
(defvar testStr2 "Hello World")
(defvar testStr3 "This is a test")
(defvar testStr4 "Hi my name is Robbie")
(defvar testStr5 "WandaVision")
(defvar testStr6 "Abed")
(defvar testStr7 "The Mandalorian")
(defvar testStr8 "Wow I learned LISP I think")

(defvar testStrList (list testStr1 testStr2 testStr3 testStr4 testStr5 testStr6 testStr7 testStr8))
(defvar shiftAmount (- 1))

(defun encrypt(testStrList)
    ;(mapcar #'pickShift' testStrList)
	(maplist (lambda (testStr) (pickShift testStr)) testStrList)
	;(pickShift(car testStrList))
	;(encrypt (rest testStrList))
)

(defun pickShift(testStr)
	(cond
	    ;((< shiftAmount 0) (posShift testStr))
	    ((< shiftAmount 0) (map 'string #'(lambda (character) (negShift character)) testStr))
		;;((> shiftAmount 0) (map 'string #' (lambda (character) (posShift(character shiftAmount)) testStr)))
		;((< shiftAmount 0) (negShift(car testStr)))
		(t (testStr))
	)
	;(pickShift(rest testStr))
)

(defun negShift(character)
	(setq asciiVal (char-code character))
	(cond 
	    ((or (char= #\A character) (char= #\a character)) (char(+ shiftAmount (+ asciiVal 26))))
		((and (char> #\A character) (char<= #\Z character)) (char(+ shiftAmount asciiVal)))
		((and (char> #\a character) (char<= #\z character)) (char(+ shiftAmount asciiVal)))
		(t (character))
	)
)

(format t "Original Strings:~%~s~%" testStrList)
(format t "Encrypted Strings:~%~s~%" (encrypt testStrList))

(write (map 'list (lambda (x) (+ x 10)) '(1 2 3 4)))

;;; ~a : shows the value
;;; ~s : shows quotes around the value
;;; ~10a : Adds 10 spaces for the value with the extra space to the right
;;; ~10Aa : Adds 10 spaces for the value with the extra space to the left
