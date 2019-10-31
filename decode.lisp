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
			do(if (or (spell-checker-1 (decipher word alphabet) word-hashmap) (equal word nil))
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
	(let ((permutations (append alphabet nil)) (frequent-letters (analyze-frequency paragraph)))
		(if (equal (length alphabet) 26)
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
				(if (equal (success-rate paragraph (add-frequent-letters frequent-letters alphabet) word-hashmap) (length paragraph))
					(and (setq permutations (append (add-frequent-letters frequent-letters alphabet) nil)) (return ))
					(if (< (success-rate paragraph (add-frequent-letters frequent-letters permutations) word-hashmap) (success-rate paragraph (append (add-frequent-letters frequent-letters alphabet) nil) word-hashmap))
						(setq permutations (append (add-frequent-letters frequent-letters alphabet) nil))
					)	
				)
				(if (> i 0)
					(rotatef (nth (- i 1) alphabet) (nth j alphabet))
				)
				(setq alphabet (reverse-list alphabet i (- (length alphabet) 1)))
			)
		))
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

; *Precondition: Takes a paragraph as input.
;
; *Postcondition: Calculates most frequent encoded six letters then
; returns list of these letters according to frequency order.
(defun analyze-frequency(paragraph)
	(let ((frequent-letters (make-hash-table :test 'equal)) (unique-letters (list )))
		(loop for word in paragraph
			do(loop for letter in word
				do(and (if (gethash letter frequent-letters)
					(setf (gethash letter frequent-letters) (+ (gethash letter frequent-letters) 1))
					(setf (gethash letter frequent-letters) 1))
					(if (not (member letter unique-letters))
						(setq unique-letters (append unique-letters (list letter)))
					)
				)
			)
		)
		(sort-as-list frequent-letters unique-letters)
	)
)

; *Precondition: Takes a hash table that has letters as key and frequency of these
; letters as value, and these letters as a unique list.
;
; *Postcondition: Six most frequent letter as list.
(defun sort-as-list(frequent-letters unique-letters)
	(loop for i from 0 to (- (length unique-letters) 1)
		do(let ((min-index i))
			(loop for j from (+ i 1) to (- (length unique-letters) 1)
				do(if (< (gethash (nth j unique-letters) frequent-letters) (gethash (nth min-index unique-letters) frequent-letters))
					(setq min-index j)
				)
			)
			(setq temp (nth min-index unique-letters))
			(setf (nth min-index unique-letters) (nth i unique-letters))
			(setf (nth i unique-letters) temp)	
		)
	)
	(reverse (last unique-letters 6))
)

; *Precondition: Takes a list, an element to add list and its position to add.
;
; *Postcondition: Adds given elements to given indexes in the list.
(defun insert-at (given-list index element)
	(let ((retval nil))
    	(loop for i from 0 to (- (length given-list) 1) 
			do(when (= i index)
        		(push element retval)
			)
      		(push (nth i given-list) retval)
		)
    	(when (>= index (length given-list))
      		(push element retval))
    	(nreverse retval)
	)
)

; *Precondition: Takes a most frequent-letters list that includes most frequent 6 letters
; in the paragraph and a 20 letters alphabet.
;
; *Postcondition: Adds most frequent letters to alphabet then returns alphabet.
(defun add-frequent-letters(frequent-letters alphabet)
	(setq alphabet (insert-at alphabet 0 (nth 2 frequent-letters)))
	(setq alphabet (insert-at alphabet 4 (nth 0 frequent-letters)))
	(setq alphabet (insert-at alphabet 8 (nth 4 frequent-letters)))
	(setq alphabet (insert-at alphabet 13 (nth 5 frequent-letters)))
	(setq alphabet (insert-at alphabet 14 (nth 3 frequent-letters)))
	(setq alphabet (insert-at alphabet 19 (nth 1 frequent-letters)))
	alphabet
)

;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph alphabet dictionary-file)
	(let ((dictionary (read-as-hashmap (coerce dictionary-file 'string))) (string-paragraph ""))
		(setq char-list (create-permutations paragraph alphabet dictionary))
		(loop for word in char-list
			do(setq string-paragraph (concatenate 'string string-paragraph (coerce word 'string) " "))
		)
		string-paragraph
	)
)

(defun Gen-Decoder-B-0 (paragraph alphabet dictionary-file)
  	(let ((dictionary (read-as-hashmap (coerce dictionary-file 'string))) (string-paragraph ""))
		(setq char-list (create-permutations paragraph alphabet dictionary))
		(loop for word in char-list
			do(setq string-paragraph (concatenate 'string string-paragraph (coerce word 'string) " "))
		)
		string-paragraph
	)
)

(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

(defun Code-Breaker (document decoder alphabet dictionary)
  	(let ((paragraph (read-as-list document))) 
	  	(mapcar decoder (list paragraph) (list alphabet) (list dictionary))
	)
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(let ((doc (read-as-list "document1.txt"))
		  (semi-alphabet '(#\b #\c #\d #\f #\g #\h #\j #\k #\l #\m #\p #\q #\r #\s #\u #\v #\w #\x #\y #\z))
		  (alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) 
		  (dictionary "dictionary2.txt")
		)
		(print "Which decoder do you want to test?")
		(print "1. Gen-Decoder-A")
		(print "2. Gen-Decoder-B-0")
		(print "3. Gen-Decoder-B-1")
		(print "Choice: ")
		(setq choice (read))
		(if (not (or (equal choice 1) (equal choice 2) (equal choice 2)))
			(print "Invalid choice!")
			(or (if (equal choice 1)
					(and (print "Gen-Decoder-A is running...")
						(print "Deciphered message: ")
						(print (Code-Breaker "document1.txt" #'Gen-Decoder-A alphabet (coerce dictionary 'list)))
					)
				)
				(if (equal choice 2)
					(and (print "Gen-Decoder-B-0 is running...")
						(print "Deciphered message: ")
						(print (Code-Breaker "document1.txt" #'Gen-Decoder-B-0 semi-alphabet (coerce dictionary 'list)))
					)
				)
				(if (equal choice 3)
					(and (print "Gen-Decoder-B-1 is running...")
						(print "Deciphered message: ")
						(print (Code-Breaker "document1.txt" #'Gen-Decoder-B-1 semi-alphabet (coerce dictionary 'list)))
					)
				)
			
			)
		)	
	)
	
)

;; test code...
(test_on_test_data)