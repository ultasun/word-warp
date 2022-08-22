;; Meghan Hollenbach & ultasun
;; Common Lisp Word Scramble Game
;; 11-23-11
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;				PARAMETER DEFINITIONS BELOW
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; the wordlist is populated with a dictionary file (later in the code)
;; the more words you have, the faster (init-game) completes!
(defparameter *wordlist* nil)
;; the collection of characters to choose from randomly
(defparameter *valid-chars* nil)
;; how many words match-chars-words() has to find in order for it's
;; proposed game to be "playable"
(defparameter *playable* 4)
;; minimal word length
(defparameter *min-word-length* 4)
;; the player characters - characters that are in the words to solve
;; (given to user at run-time)
(defparameter *player-chars* nil)
;; the words the player must find for the game to be completed
(defparameter *player-words* nil)
;; new list of hyphens representing a parallel array of the words
(defparameter *words-hyphened* nil)


;;------------------------------------------------------------------------------
;; 		DICTIONARY TO FILESYSTEM FUNCTIONS
;;------------------------------------------------------------------------------

;; load a dictionary file from a text file. the words in the text file should
;; be one entry per line, and sorted in alphabetical order.

;; load-dictionary-txt (n filename) 
;; params: n chars for max sized word
(defun load-dictionary-txt (n filename)
  (let ((w '()))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'foo)
            until (eq line 'foo)
            do
            (and 
             ;; test the length of this word 
             (if (>= (length line) *min-word-length*) line ())
             (if (<= (length line) n) line ())
             ;; apostrophied words require ||'s to be symbols.
             ;; ||'s aren't compatible as letters in our game.
             ;; dictionary files tend to have apostrophied entries
             (if (find #\' line :test #'equal) () 
		 ;; convert the read string into a symbol
                 (push  
                  (string-upcase line) w))))
      ;; merges new wordlist with existing wordlist without creating
      ;; duplicate entries
      ;; *wordlist* becomes unsorted, however
      (setf *wordlist*
	    (sort-strings
	     (set-difference w *wordlist*))))))

;;------------------------------------------------------------------------------
;;			 FUNCTION DEFINITIONS : UTILITY FUNCTIONS	
;;------------------------------------------------------------------------------
;; implode (l)
;; params: l is a list
;; implode a list of strings into a single string
(defun implode (l)
  (let
      ((word nil))
    (loop for x in l do (setf word (concatenate 'string word x))) word))

;; explode (e)
;; params: e is a string
;; Explode a string into a list of single character strings
(defun explode (e)
  (let ((chars nil))
    (loop for i below (length e) do
	  (push (subseq e i (1+ i)) chars))
    (reverse chars)))

;; string-to-symlist (l) 
;; params: l is the string accepted by the function
;; loop through each single char string (after l is exploded) 
;; collect returns the list of symbols intern has created
(defun string-to-symlist (l)
  (loop for i in (explode l) collect (intern (string-capitalize i))))

;; symlist-to-string (l)
;; params: l is the symbol being passed
;; (defun symlist-to-string (l)
;; (let ((w "")) 
;; set i equal to each single char string in l, and
;; set w equal to w+i concat
;; (loop for i in l do (setf w (concatenate 'string w (string i)))) w))

;; countchar (c str)
;; params: c is the character you're interested in, str is the word it's
;; evaluating against 
;; counts how many times a character shows up in a word
(defun countchar (c str)
  (let ((n 0))
    (loop for x across str do
	  (if
	   (string= x c)
	   (setf n (1+ n))))
    n))

;; sort-string (l)
;; params: l is the list of strings
;; alphabetically sorts the strings with
;; (sort l (function to define how to sort))
(defun sort-strings (l)
  (sort l (lambda (x y) (string< x y))))

;;------------------------------------------------------------------------------
;;                         FUNCTION DEFINITIONS: GAME FUNCTIONS
;;------------------------------------------------------------------------------

;; init-valid-chars ()
;; find a list of non-repeating characters of the words from our wordlist 
;; loops through every word extracted from the dictionary finding chars that
;; have not been found yet
(defun init-valid-chars ()
  (let ((alphabet nil)) ;; create a local alphabet variable = nil
    (dolist (x *wordlist*) ;; for every string in the word list put in x
      (dolist (y (explode x)) ;; for every single character string put in y
	;; next line would disable repeats from appearing in the alphabet
        ;;(if (set-difference (list (intern y)) alphabet)
	(push y alphabet))) ;;) ;; push new symbol in y into the alphabet var
    ;; set *valid-chars* equal to the strings in alphabet
    (setf *valid-chars* (sort-strings alphabet)))) 

;; random-char () 
;; get a random character  from the characters extracted from the *wordlist*
;; nth returns the single char string
(defun random-char () (nth (random (length *valid-chars* )) *valid-chars*))

;; random-chars (n)
;; params: n is the # of random characters to get
;; get n random charactters by calling the random-char function
(defun random-chars (n)
  (let ((rchars nil) (rchar nil))
    (loop until
	  (= (length rchars) n) do
	  (progn (setf rchar (random-char))
		 (push rchar rchars)))
    rchars))

;;------------------------------------------------------------------------------
;; match-chars-words (chars)
;; params: chars 
;; from our list of valid letters, find at least *playable* words to play with.
;; return nil if no such case was found (see init-game)
(defun match-chars-words (chars)
  ;; set chars-fit to True and charstr to the string of valid-chars
  (let ((playable-words nil) (chars-fit T) (charstr chars)) 
    ;; loop through *wordlist* giving each word to w
    (loop for w in *wordlist* do
	  (progn 
	    ;; loop across the string w and give each value to i
	    (loop for i across w do
		  ;; if i's position in w is <= i's position in charstr
		  ;; in other words, make sure that every char in the word is
		  ;; in the accepted char list
		  ;; (we play with every word until we prove we can't)
		  (if
		   (<=
		    (countchar i w) (countchar i (implode charstr)))
		   () ;; do nothing
		   (setf chars-fit nil)) ; else set clone to nil
		  
		  ) ;; end inner loop
	    ;; if there's a clone 
	    (if chars-fit (push w playable-words)) 
	    (setf chars-fit T)
	    )
	  ) ;; end loop
    ;; is this words list any good to play with? 
    ;; enough words to play?
    (if
     (> (length playable-words) *playable*) 
     (setf *player-words* playable-words) nil)))

;; list-contains-string (str wordlist)
;; params: str is the string we are comparing, wordlist is the list we compare
;; str to
;; small utility function...does the wordlist have the string str in it? 
;; this is case sensitive...
;; returns T if str is a member of wordlist
(defun list-contains-string (str wordlist) 
  (let ((match nil))
    (dolist (x wordlist)
      (if (string= (string-upcase x) str) (setf match T)))
    match))

;; init-game (n)
;; params: n is the amount of characters being used
;; search for a playable game
(defun init-game (n)
  ;; set *player-chars* equal to the randomly selected chars from the 
  ;; *wordlist* generated by the dictionary
  (setf *player-chars* (sort-strings (random-chars n)))		
  ;; loop for every *player-chars* thrown into match-chars-words (chars)
  (loop (match-chars-words *player-chars*)
   ;; check for something in *player-words*, then return to caller function
   (if *player-words* (return)
       ;; empty brackets is the else cond
       (setf *player-chars* (sort-strings (random-chars n)))
       ) ;; end if
   ) ;; end loop
  )

;; hypens (l)
;; params: n is the number of hyphens needed
(defun hyphens (n)
  (let ((hyph "---"))
    (loop for i below (- n 3) do
	  (setf hyph (concatenate 'string hyph "-")))
    ;; need to restate the local variable as the last thing done because
    ;; we are returning it
    hyph)) 

;; convert-to-hyph ()
;; loops through the *player-words* list checking the length of each word
;; once it finds the length, the loop pushes that many hyphens onto local var w
;; by calling hyphens, after the loop ends, *words-hyphened* is set to w stack
(defun convert-to-hyph ()
  (let ((w '()))
    (loop for i in *player-words* do
	  (push
	   (hyphens
	    (length
	     (explode i)))
	   w)) ;;end loop
    (setf *words-hyphened* w)))

;; check-guess (n)
;; params: n is a string of a word 
;; check if the guess is a symbol in the player-words list, if it is,
;; call the function checking with 
;; params guess start_position position_of_guessed_word
(defun check-guess (n)
  (let ((newlist ())) 
    (if (list-contains-string (string-upcase n) *player-words*) 
        (progn
	  (loop for i in *words-hyphened* 
		for j in *player-words*
		do
		(progn
		  (if (string= j n)  (push j newlist)
                      (push i newlist)) ; else
		  ))
	  ;; if n was a correct guess...
	  ;; we must make a reversed copy of the newlist var 
	  ;; set the previous *words-hyphened* to a revised list
	  ;; including the correct guess
          (setf *words-hyphened* (reverse newlist))))))

;; get-hint ()
;; pick a random word from the words the player hasn't found yet
(defun get-hint () 
  ;; let var unknown equal everything that's not in *words-hyphened*
  ;; but in *player-words*
  (let ((unknown
	 (set-difference
	  *player-words* 	
	  ;; take out all hyphened words (unknown)
	  ;; you'll only have these 5 hyphen lengths
          (set-difference
	   *words-hyphened*
	   '(--- ---- ----- ------ -------)
	   :test 'equal)))) 
    ;; return a string in the (random) position in unknown
    (nth (random (length unknown)) unknown)))

;; hinter ()
;; give the player a hint by filling in one of the words for them
;; calls check-guess with get-hint's return value
(defun hinter ()(check-guess (get-hint)))

;; run-game ()
;; run-game holds all of the stmts to initialize a new game
(defun run-game () 
  ;; print user friendly statements
  (print "------------------------------------")
  (print "...Your words to guess are...")
  ;; loop through every hyphened word printing them to console
  (loop for i below (length *words-hyphened*) do
	(print (nth i *words-hyphened*))) 
  ;; if all hyphened words have been found
  ;; (i.e. match everything in *player-words*)
  ;; print you've won
  ;; else initiate a guessing sequence
  (if (equal *words-hyphened* *player-words*) 
      (print "...You have WON the game!") 
      (progn 
	(print "...You have to guess with the characters...")
	(print *player-chars*) ;; print valid chars in words
	(print "...Take a guess! Or ? for a hint!...")
	(fresh-line)
	;; read in the user input
        (let ((in (string-upcase (read-line))))
          (cond ((string=  in "?") ;; '?' is help case - gives word
                 (hinter)) ;; calls hinter () for word
		((check-guess in) ;; calls check-guess (input)
		 (print "...Correct guess!"))
		(T (print "...Incorrect guess!"))))
	(run-game) ;; recursive call to run the game again !
	))
  nil)

;;------------------------------------------------------------------------------
;; start-me (n) ************MAIN FUNCTION****************
;; in console: (start-me n) 
;; params: n is the number of letters the game should use for the words found.
;; ex: n = 6 ... max size word you can have is 6 letters.  
;; number of words for the user to unscramble, ranging from 4-6 letters
(defun start-me (n)
  (load-dictionary-txt n "english.0") 
  (init-valid-chars)
  (init-game n)	 
  (convert-to-hyph)				 
  (setf *player-words* (reverse *player-words*))
  (run-game))

;;------------------------------------------------------------------------------
;;-------------- NOTES ---------------------------------------------------------
(start-me 4)

;;------------------------------------------------------------------------------
;;EXTRA FUNCTIONS
;;------------------------------------------------------------------------------
;; r is the random symbol sequence and str is the string
;;(defun random-chars-fit-word (r str)
;;  (let ((nomatch nil) (letters (explode r)) (expl-str (explode str)))
;;  (loop for i in letters do
;; (if (intersection (list (intern i)) r) (setf nomatch T)) )(not nomatch)))

;;------------------------------------------------------------------------------
;;get n random words
;; (defun random-words (n)
;; (let ((words nil))
;; (loop for i below n do
;; (push (nth (random (length *wordlist*)) *wordlist*) words)) words))
;;------------------------------------------------------------------------------
;; Save the *wordlist* to the local disk for later use
;; (defun save-db (filename)
;; (with-open-file (out filename 
;; :direction :output
;; :if-exists :supersede)
;; (with-standard-io-syntax
;; (print *wordlist* out))))
;;Load a *wordlist* from the local disk
;; (defun load-db (filename)
;; (with-open-file (in filename)
;; (with-standard-io-syntax
;; (setf *wordlist* (read in)))))

