;; Meghan Hollenbach & ultasun
;; Common Lisp Word Scramble Game
;; 11-23-11
;;-----------------------------------------------------------------------------
;;				PARAMETER DEFINITIONS BELOW
;;-----------------------------------------------------------------------------
;; a demo word list. 
;; the more words you have, the faster (init-game) completes...to an extent.
(defparameter *demo-wordlist*
  '(abe ape are fat mat oat pat pin tar tin tow war win yen able bare bars
    bear beer care cars chin fake hair hare lake make mars rake ship tape
    tear word grape paint shape share sheer zebra toward chicken guitars))

(defparameter *wordlist* nil)

;; the collection of characters to choose from randomly
(defparameter *valid-chars* nil)

;; how many words match-chars-words has to find in order for its
;; proposed game to be "playable"
(defparameter *playable* 4)
;; the player characters
(defparameter *player-chars* nil)
;; the words the player must find
(defparameter *player-words* nil)
;; new list of words player must find but each word's cons link is to
;; it's hyphen structure
(defparameter *words-hyphened* nil)


;;-----------------------------------------------------------------------------
;; 		DICTIONARY TO FILESYSTEM FUNCTIONS
;;-----------------------------------------------------------------------------
;; load a dictionary file from a text file. the words in the text file should
;; be one entry per line, and sorted in alphabetical order.
;; *TODO* a mergesort on existing *wordlist* entries and new ones from filename,
;;        without repeating entries!
(defun load-dictionary-txt (n filename)
  (let ((w '()))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'foo)
            until (eq line 'foo)
            do
            (and 
             ;; test the length of this word 
             (if (>= (length line) 4) line ())
             (if (<= (length line) n) line ())
             ;; apostrophied words require ||'s to be symbols.
             ;; ||'s aren't compatible as letters in our game.
             ;; dictionary files tend to have apostrophied entries
             (if
	      (find #\' line :test #'equal) ()
	      (progn 
		;; convert the read string into a symbol
		(push
		 (implode
		  (cdr
		   (explode 
		    (intern
		     (string-upcase line)
		     "KEYWORD"))))
		 w)) ))))
    ;; merges new wordlist with existing mergelist without creating duplicate
    ;; entries... *wordlist* becomes unsorted, however
    (setf
     *wordlist*
     (sort-symbols
      (set-difference w *wordlist*)))))

;; Save the *wordlist* to the local disk for later use
(defun save-db (filename)
  (with-open-file (out filename 
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *wordlist* out))))
;; Load a *wordlist* from the local disk
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *wordlist* (read in)))))

;;------------------------------------------------------------------------------
;;			 FUNCTION DEFINITIONS : UTILITY FUNCTIONS	
;;------------------------------------------------------------------------------

;; Implode takes a list of symbols and returns a single symbol
(defun implode(l)
  (read-from-string
   (coerce (mapcar #'(lambda(s)(char(symbol-name s)0))l)'string))) 

;; Explode takes a symbol and returns a list of symbols
(defun explode(e)
  (mapcar #'(lambda(x)(intern(string x)))
          (coerce(prin1-to-string e)'list)))

;; Sort a list of symbols
(defun sort-symbols (l)
  (sort l (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

;; Game-print accepts a list of symbols and prints the list on one line,
;; with spaces
(defun game-print (lst)
  (princ (coerce (coerce (string-trim "() "
				      (prin1-to-string lst))
			 'list)
		 'string))
  (fresh-line))

;; Game-read reads tokens from the user and returns a list containing symbols
;; of the tokens
(defun game-read ()
  (read-from-string 
   (concatenate 'string "(" (read-line) ")")))

;;------------------------------------------------------------------------------
;;                         FUNCTION DEFINITIONS: GAME FUNCTIONS
;;------------------------------------------------------------------------------

;; find a list of non-repeating characters of the words from our wordlist 
(defun init-valid-chars ()
  (let ((w '()))
    (loop for i in *wordlist* do ;; goes through each word in the wordlist
          (loop for j in (explode i) do ;; goes through each symbol in the list
		(if
		 (not
		  (intersection
		   (list j) w)) ;if the symbol in j is not in the w stack 
		 (push j w) ;push it on
		 ())))
    ;; set the valid-chars (global var) equal to everything in w  
    (setf *valid-chars* w))) 

;; get a random character 
(defun random-char () (nth (random (length *valid-chars* )) *valid-chars*))

;; get n random characters
(defun random-chars (n) 
  (let ((rchars nil) (rchar nil) )
    (loop until (= (length rchars) n) do
	  (progn 
            (setf rchar (random-char))
            (if
	     (intersection (list rchar) rchars) () (push rchar rchars))))
    rchars)) 

;; get n random words
(defun random-words (n)
  (let ((words nil))
    (loop for i below n do
	  (push (nth (random (length *wordlist*)) *wordlist*) words))
    words))

;; given the quantity N letters we want to play with, find N random letters
;; from our list of valid letters, and find at least *playable* words to play
;; with. return nil if no such case was found (see init-game)
(defun match-chars-words (n) 
  (let ((w '())) 
    (setf *player-chars* (random-chars n)) 
    (loop for i in *wordlist* do 
          (if (progn (not (set-difference (explode i) *player-chars*))) 
              (push i w)
              ()))
    (if (> (length w) *playable*) 
        (setf *player-words* w) 
        ())))

;; search for a playable game
(defun init-game (n)
  (setf *player-words* nil)
  (loop (match-chars-words n)
   (if *player-words*
       (return)
       () )))

(defun hyphens (l)
  (cond ((eq l 3) '--- )
	((eq l 4) '---- )
	((eq l 5) '----- )
	((eq l 6) '------ )
	((eq l 7) '------- )))

(defun convert-to-hyph () 
  (let ((w '()))
    (loop for i in *player-words* do 
	  (cond 
	    ((eq (length (explode i)) 3) (push (hyphens 3) w) ) 
	    ((eq (length (explode i)) 4) (push (hyphens 4) w) )
	    ((eq (length (explode i)) 5) (push (hyphens 5) w) )
	    ((eq (length (explode i)) 6) (push (hyphens 6) w) )
	    ((eq (length (explode i)) 7) (push (hyphens 7) w) )
	    (t (print i))))
    (setf *words-hyphened* w)))

(defun checking (n w i j)	
  (if (= i j)
      (cons (setf (nth i *words-hyphened*) n) (cdr w))
      (checking n (cdr w) (+ 1 i) j)))

;; check if the guess is a symbol in the player-words list, if it is,
;; call the function checking with params guess start_position
;; position_of_guessed_word
(defun check-guess (n) 
  (if (intersection *player-words* n) 
      (checking
       (car n) *words-hyphened* 0
       (position (car n) *player-words*))))

;; pick a random word from the words the player hasn't found yet
(defun get-hint () 
  (let
      ((unknown
	(set-difference
	 *player-words* ;; only have these 5 hyphen lengths
         (set-difference
	  *words-hyphened*
	  '(--- ---- ----- ------ -------)))))
    (nth (random (length unknown)) unknown)))

;; give the player a hint by filling in one of the words for them
(defun hinter ()
  (check-guess (list (get-hint))))

;; run-game holds all of the stmts to initialize a new game
(defun run-game () 
  (game-print '(...your words to guess are...))
  (loop for i below (length *words-hyphened*) do
	(game-print
	 (nth i *words-hyphened*)))
  (if
   (equal *words-hyphened* *player-words*) 
   (game-print '(...you have won the game!)) 
   (progn 
     (game-print '(...You have to guess with the characters...))
     (game-print *player-chars*)
     (game-print '(...Take a guess! Or ? for a hint!...))
     (let ((in (game-read)))
       (cond ((eql (car in) '?) 
              (hinter)) 
	     ((check-guess (list (car in))) 
	      (game-print
	       '(...Correct guess!)))
	     (T (game-print '(...Incorrect guess!)))))
     (run-game))))

;; "play" word-warp...this is the main function
;; using a cond statement to error check input for game symbol length
(defun start-me (n)
  (game-print '(...loading...))
  (load-dictionary-txt n "english.0")
  (init-valid-chars)
  (init-game n)
  (convert-to-hyph)
  (setf *player-words* (reverse *player-words*))
  (game-print ;; notice the back-quote
   `(...welcome to ,n letter word warp...))
  (run-game))

;; once a word is entered ...
;; if it's a word in the list, replace the hyphens with the word typed by
;; the user
;;------------------------------------------------------------------------------
;; PROGRAM START AREA 
;;------------------------------------------------------------------------------
;; start the game for three letters
(start-me 4)

;;------------------------------------------------------------------------------
;; EOF word-warp-symbols.lisp


