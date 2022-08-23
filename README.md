# *Word Warp* in *Common Lisp*
This was a pair programming project conducted as a homework assignment during Bloomsburg University's 'Organization of Programming Languages' course.  I am one of two authors.  The course was instructed by Dr. William Calhoun.

*Word-Warp* is a game where you descramble the letters.  

# Two Versions

The difference between `word-warp.lisp` and `word-warp-symbols.lisp` is, 
* The former uses strings 
* The latter uses symbols
 
The implication of these differences, is that:
* Strings have separate case, and separate methods to compare them as desired.
* Common Lisp symbols are capitalized by the interpreter (by default.)

# Playing
## Automatic with *Docker*
A *Docker* image is available, the game will start using four letters...

`docker run --rm -it ultasun/word-warp`

...the game is very difficult with four letters, due to the 100,000 word dictionary installed.  The user would be mad to try any higher number...
## Manual installation
### Want to play more than four letters?
Clone the repository, adjust the last line of `word-warp-symbols.lisp`, the function call to `START-ME`, [line 266](https://github.com/ultasun/word-warp/blob/master/word-warp-symbols.lisp#L266).  Beware, playing with more than four letters, with the 100,000 word dictionary, takes a long time to win.

`clisp word-warp-symbols.lisp`

The game should work with [*GNU Clisp*](https://clisp.sourceforge.io/) or [*Armed Bear Common Lisp*](https://armedbear.common-lisp.dev/).

# Credits
Written by Meghan Hollenbach and ultasun, November 2011.  See the `LICENSE`.

This was the first major *Common Lisp* program written by either author.

Thank you for reading!