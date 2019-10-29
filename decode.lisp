; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Tolga Reis                       *
; *********************************************

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"

; *Precondition: Takes a filename, reads it and collects words in the file 
; as list of character list.

; *Postcondition: Returns a list of character list.
(defun read-as-list (filename)
	(with-open-file (stream filename)
  		(let ((words (list )) (word (list )))
			(loop for character = (read-char stream nil)
        		while character
        		do (if (or (eql #\Space character) (eql #\Newline character))
						(and (setq words (append words (list word))) (setq word nil))
						(and (setq word (append word (list character))))
				)
			)
			(and (setq words (append words (list word))) (setq word nil))
			words
		)
	)	
)

; *Precondition: Takes a filename to read.
;
; *Postcondition: Returns a list of word string.
(defun read-as-word-list(filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line
		)
	)
)

; *Precondition: Takes a filaname to read.
;
; *Postcondition: Returns a hashmap that includes words as values 
; and hash values as keys.
(defun read-as-hashmap(filename)
	(let ((word-hashmap (make-hash-table :test 'equal)) (word-list (read-as-word-list filename)))
		(loop for word in word-list
			do(setf (gethash (sxhash word) word-hashmap) word)
		)
		word-hashmap
	)
)

;; -----------------------------------------------------
;; HELPERS
;; -----------------------------------------------------

; *Precondition: Takes a word to check whether is in dictionary or not,
; and a word list which includes dictionary.
;
; *Postcondition: Returns t when the word is in the dictionary, 
; nil otherwise
(defun spell-checker-0 (word word-list)
	(let ((new-word (coerce word 'string)))
		(if (equal nil (member new-word word-list :test #'string-equal))
			(not (not nil))
			(not nil)
		)
	)
)

; *Precondition: Takes a word and a word hashmap that each word in the
; dictionary has a hash key.
;
; *Postcondition: Checks whether the hashcode of the given words is in 
; word hashmap; if it is then checks whether the word is searched word
; then returns t, nil otherwise.
(defun spell-checker-1 (word word-hashmap)
 	(let ((new-word (coerce word 'string)))
	 	(if (equal nil (gethash (sxhash new-word) word-hashmap))
			(not (not nil))
			(if (equal new-word (gethash (sxhash new-word) word-hashmap))
				(not nil)
				(not (not nil))
			) 
		)
	)
)

; *Precondition: Takes a ciphered word and a character list that is alphabet.
;
; *Postcondition: Returns a word that is deciphered according to alphabet. 
(defun decipher(word alphabet)
	(let((new-word (list nil)))
		(loop for character in word
			collect (i2c (position character alphabet))
		)
	)
)

; *Precondition: Takes an encrypted paragraph and a cipher alphabet.
;
; *Postcondition: Deciphers whole paragraph with decipher function then returns it
(defun decipher-paragraph(paragraph alphabet)
	(loop for word in paragraph
		collect (decipher word alphabet)
	)
)
; *Precondition: Takes a list and two position.
;
; *Postcondition: Reverses element order between given positions then returns
; the list.
(defun reverse-list(char-list position-1 position-2)
	(loop while (< position-1 position-2)
		do(or (rotatef (nth position-1 char-list) (nth position-2 char-list))	
			  (not (setq position-1 (+ position-1 1)))
			  (not (setq position-2 (- position-2 1)))	 
		)
	)
	char-list
)

; *Precondition: Takes encrypted paragraph, cipher alphabet and dictionary hashmap.
;
; *Postcondition: Decipher paragraph and calculates success rate of cipher alphabet,
; then returns it. 
(defun success-rate(paragraph alphabet word-hashmap)
	(let ((counter 0))
		(loop for word in paragraph
			do(if (spell-checker-1 (decipher word alphabet) word-hashmap)
				  (setq counter (+ counter 1))
			)
		)
		counter	
	)
)

; *Precondition: Takes an alphabet as a list.
;
; *Postcondition: Finds all permutations of the given list by list length,
; then returns a list that includes all permutations of the given list.
(defun create-permutations(paragraph alphabet word-hashmap)
	(let ((loop-bool t) (permutations (append alphabet nil)))
		(loop for n from 1 to (factorial (length alphabet))
			do(let ((i (- (length alphabet) 1)) (j (- (length alphabet) 1)))
				(loop while (>= (c2i (nth (- i 1) alphabet)) (c2i (nth i alphabet)))
					do(if (equal (setq i (- i 1)) 0)
						(return )
					)
				)
				(if (> i 0)
					(loop while (and (> j i) (<= (c2i (nth j alphabet)) (c2i (nth (- i 1) alphabet))))
						do(setq j (- j 1))
					)
				)
				(if (equal (success-rate paragraph alphabet word-hashmap) (length paragraph))
					(and (setq permutations (append alphabet nil)) (return ))
					(if (< (success-rate paragraph permutations word-hashmap) (success-rate paragraph (append alphabet nil) word-hashmap))
						(setq permutations (append alphabet nil))
					)	
				)
				(if (> i 0)
					(rotatef (nth (- i 1) alphabet) (nth j alphabet))
				)
				(setq alphabet (reverse-list alphabet i (- (length alphabet) 1)))
			)
		)
		(decipher-paragraph paragraph permutations)
	)	
)

; *Precondition: Takes a number as an argument.
;
; *Postcondition: Calculates its factorial then returns it.
(defun factorial(n)
       (let ((fact 1))
            (do ((i 1 (+ i 1) )) ((> i n) fact)
                (setf fact (* fact i))
            )  
       ) 
)
;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph alphabet dictionary-file)
	(let ((dictionary (read-as-hashmap dictionary-file)) (string-paragraph ""))
		(setq char-list (create-permutations paragraph alphabet dictionary))
		(loop for word in char-list
			do(setq string-paragraph (concatenate 'string string-paragraph (coerce word 'string) " "))
		)
		string-paragraph
	)
)

(defun Gen-Decoder-B-0 (paragraph)
  	;you should implement this function
)

(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

(defun Code-Breaker (document decoder)
  	;you should implement this function
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(let ((doc (read-as-list "document1.txt")) 
		  (word '(#\f #\d #\a #\e #\b #\c #\h #\g)) 
		  (word-list (read-as-word-list "dictionary2.txt"))
		  (word-hashmap (read-as-hashmap "dictionary2.txt"))
		)
		(setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))	
		(print (Gen-Decoder-A doc alphabet "dictionary2.txt"))	
	)
	
)

;; test code...
(test_on_test_data)