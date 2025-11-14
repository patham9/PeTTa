;**********************************************************************
;
;   Greedy Chess 
;
;   Mettalog Project 2024-2025
;
;   Function:  Play chess (human vs MeTTa program) using a fairly simple "greedy" approach
;              with moves that do not project possible boards beyond the present board.
;   Input:     User's commands and moves from console.
;   Output:    Chess board displayed to console with computer's move.
;
;   10/25      Modified for chess tutorial running under PeTTa.
;
;**********************************************************************
; * Redistribution and use in source and binary forms, with or without
; * modification, are permitted provided that the following conditions
; * are met:
; *
; * 1. Redistributions of source code must retain the above copyright
; *    notice, this list of conditions and the following disclaimer.
; *
; * 2. Redistributions in binary form must reproduce the above copyright
; *    notice, this list of conditions and the following disclaimer in the
; *    documentation and/or other materials provided with the
; *    distribution.
; *
; * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; * POSSIBILITY OF SUCH DAMAGE.
;**********************************************************************
;
;   Original author:  Mike Archbold 2024, 2025
;
;   DATA STRUCTURES
;
;   The chessboard data structure that persists between moves through the game is a list with each sublist
;   a square on the board. Each square is unique with ordinary cartesian x/y coordinates on 8x8 grid.
;   There are two colors in play:  gold (AI) and silver (human). The structure of the list is eg:
; 
;   Main chessboard list atom example:  
;     (board-state (  (1 1 s r) (1 2 s n).... (4 4).... (1 8 g r)... )) with 64 sublists
;
;   Note that squares occupied by a piece have a length of 4 with the color and rank. Squares with
;   no occupying piece are only two in length with just x/y coordinates. Thus simply determining the
;   length of the square sublist will show whether or not the square is occupied.
;   
;   While moves are taking place, Greedy Chess "envisions" possible moves. At the onset of the move,
;   either AI or human, the chessboard list as described above is converted to individual atoms of the form:
; 
;     (square x y color rank) and (square x y) to denote both occupied and unoccupied squares.
;
;   These 'square' type atoms only exists for one move. The permanent move is stored in the larger list
;   as described above: the main chessboard list atom is the main record of play for the whole game.
;
;   PROCESSING
;   
;   The game proceeds as a normal chess game with players taking turns. The 'square' type atoms are 
;   first created at the outset of each side's move based upon the chessboard list.
;
;   Possible moves are evaluated by rapidly adding and removing these 'square' atoms. As a design
;   consideration use of such atoms harmonizes well with the 'atomic' structure of MeTTa which is 
;   not really the same as LISP type list processing. At the start of the move, the transient 'square' atoms
;   are loaded into atomspace and at the conclusion of the move the 'square' atoms are used to 
;   recreate the chessboard. Take care that after "envisioning" moves you move the 'square' atoms
;   back to their original position. It is only at the very conclusion of the move that the official
;   move updates the 'square' atoms. 
;
;   Again: dynamic envisioning of moves is done using the 'square' atoms but as soon as the move has
;   been envisioned, move the 'square' atoms back to their original position as it was at the start of the move.
;   If this is not done asynchronous update problems might result. Also note that other transient
;   atoms are created for move envisioning.
;
;   NOTES
;
;   During programming tail recursion functions were often created, but it was discovered that these
;   were often faster instead using metta calls to 'match'. The original functions have sometimes been retained for
;   comparison. In general the defining features of MeTTa are the use of atomspace, 'match' 'superpose' and
;   'collapse'. Occasionally it is wise to use 'xor' in lieu of 'or' (if possible) to avoid unexpected
;   nondeterministic behavior. In general it is not necessary to think in GOFAI chaining terms (forward, backward).
;
;   ENHANCEMENTS POSSIBLE
;
;   Many chess games exist. The objective of this program has been to exercise the Mettalog 
;   processing substrate for shaking out bugs and speed testing. The program was a translation from an old Prolog 
;   program called "Deep Blue Dummy." These programs are functional but not totally complete chess games.
;
;   Many enhancements are possible:
;
;   1) en passant pawn moves
;   2) castling
;   3) reward for advancing pawn to opponent's end line
;   4) (COMPLETED) use of the standard letters on the x axis 
;   5) tuning and more sophisticated moves of all types
;

!(import! &self ../lib/lib_metta4)                  ; needed for 'progn'

;*******************************************************
; Global type definitions
;*******************************************************

;(: game-state (-> Expression Atom))        ; create an atom for game state (eg., "started" "checkmate")
;(: board-state (-> Expression Atom))       ; for saving the board after each move 


; these are used to prevent unexpected non-determinism
;(: and1 (-> Atom Atom Bool))
;(= (and1 $x $y)    (and  (limit 1 $x)  (limit 1 $y)))
;(: xor1 (-> Atom Atom Bool))
;(= (xor1 $x $y) (xor  (limit 1 $x)  (limit 1 $y)))
;(: or1 (-> Atom Atom Bool))
;(= (or1 $x $y) (or  (limit 1 $x)  (limit 1 $y)))
;(: not1 (-> Atom Bool))
;(= (not1 $x)  (not  (limit 1 $x)))


;*******************************************************
; General utility functions
;*******************************************************

    
;***************************************************************
; Function:     nth
; Description:  Returns the N-th element from a list (1-based index).
;
; Input:        $n     - The position (1-based) of the element to return.
;               $list  - A list of elements (e.g., (a b c d)).
;
; Output:       The atom at position $n in the list. Returns the first element if n = 1.
;***************************************************************
(= (nth $n $list) 
    (if (== $n 1)
        (car-atom $list)
        (nth (- $n 1) (cdr-atom $list)))) ; Recursion: move to the next element (cdr-atom) and decrease n.

;***************************************************************
; Function:     contains_symbol
; Description:  Checks whether a given symbol exists in a list.
;
; Input:        $list - A list of atoms (e.g., (a b c d))
;               $sym  - A symbol to search for in the list
;
; Output:       True if $sym is found in $list; otherwise False
;***************************************************************
(= (contains_symbol $list $sym) 
    (if (== $list ())
        False
        (if (== (car-atom $list) $sym)
            True
            (contains_symbol (cdr-atom $list) $sym))))

;***************************************************************
; Function:     char_to_int
; Description:  Converts a character string "0".."9" to its corresponding integer.
;               Will convert to standard chess x axis letters too.
;
; Input:        $char - A string representing a digit ("0".."9")
;
; Output:       Integer value 0..9 if valid; otherwise NIL
;***************************************************************
(= (char_to_int $char)
    (case $char
        (
        ("1"  1)
        ("2"  2)
        ("3"  3)
        ("4"  4)
        ("5"  5)
        ("6"  6)
        ("7"  7)
        ("8"  8)
        ("a"  1)
        ("b"  2)
        ("c"  3)
        ("d"  4)
        ("e"  5)
        ("f"  6)
        ("g"  7)
        ("h"  8)
        ;otherwise
        ($_   NIL) ; <-- in case user inputs bad char, this will result in a fail.
        )))


;***************************************************************
; Function:     int_to_char
; Description:  Converts an integer 1..8 to the corresponding chess file letter.
;
; Input:        $int - Integer in range 1 to 8
;
; Output:       Corresponding file letter "a".."h", or "?" if invalid
;***************************************************************
(= (int_to_char $int)
    (case $int
        (
        (1 a)
        (2 b)
        (3 c)
        (4 d)
        (5 e)
        (6 f)
        (7 g)
        (8 h)
        ($_ ?) ; fallback for invalid input
        )))

;***************************************************************
; Function:     return_length
; Description:  Calculates the number of characters in a given symbol.
;               Converts the symbol to a character list using `stringToChars`,
;               then returns the size of that list.
;
; Input:        $symbol - A string or symbol (e.g., "a2a3")
;
; Output:       Integer representing the number of characters in the symbol.
;
; Example:      (return_length "a2a3") ⇒ 4
;
; Notes:        Useful for validating input lengths, such as move commands.
;               Also works for symbols, eg., a2a3 with no quotes.
;***************************************************************
(= (return_length $symbol)
  (let $list_from_symbol (stringToChars $symbol) (size-atom $list_from_symbol)))

;***************************************************************
; Function:     convert_x_letter
; Description:  Extracts the X axis component from a chess square and converts
;               the integer to the standard letter format
;
; Input:        $square - A string representing a chess square
;
; Output:       An expression with the converted x-coordinate (int) to a standard letter
;     
;***************************************************************
(= (convert_x_letter $square)
  (let*
    (
      ($x (nth 1 $square))         ; get the int x axis value
      ($x_ltr (int_to_char $x))    ; convert to char
      ($y (nth 2 $square))
    )
    (if (== (size-atom $square) 2 )
      ($x_ltr $y)
      (let*
        (
         ($color (nth 3 $square))
         ($rank (nth 4 $square))
        ) ;also convert to black or white color for standardization.
        (if (== $color s) 
            ($x_ltr $y white $rank)
            ($x_ltr $y black $rank)
        )))))


;***************************************************************
; Function:     concat_lists
; Description:  Concatenates a list of sublists into a single flat list.
;               Only performs a shallow (1-level) flattening.
;
; Input:        $ListofLists - A list of sublists (e.g., ((1 2) (3 4) () (5)))
;
; Output:       A single flattened list (e.g., (1 2 3 4 5))
;***************************************************************
(= (concat_lists $ListofLists)
  (if (== $ListofLists () )
      ()
      (let $first_list (car-atom $ListofLists)
        (if (== $first_list ())
            (concat_lists (cdr-atom $ListofLists))
            (let*
              (
              ($a (car-atom $first_list))   ;$a = first element first list
              ($b (cdr-atom $first_list))   ;$b = rest of first list
              ($c (cdr-atom $ListofLists))  ;$c = all the remaining lists
              ) ;$d = remaining elements of first list and rest of lists
                ;$f = the concatenation of the rest of the lists
                ;we return the first element of the first list and concatenation of the remainder.
              (let $d (cons-atom $b $c) (let $f (concat_lists $d) (cons-atom $a $f )))  
            )))))

;***************************************************************
; Function:     addit_list
; Description:  Computes the sum of a list of integers.
;
; Input:        $integer_list - A list of integers (e.g., (1 2 3 4))
;
; Output:       The total sum of all integers in the list (e.g., 10)
;***************************************************************
(= (addit_list $integer_list)
    (if (== $integer_list ())
        0
        (+ (car-atom $integer_list) (addit_list (cdr-atom $integer_list)))))

;***************************************************************
; Function:     progq
; Description:  Executes a list of quoted expressions sequentially.
;               Each expression must be wrapped in (quote (...)).
;
; Input:        $exp - A list of quoted expressions
;               Example:
;                   (progq
;                     (
;                       (quote (function-a))
;                       (quote (function-b))
;                       ...
;                     ))
;
; Output:       Evaluates each expression in order for side effects.
;               Returns () when all are processed.
;
; Note:         Similar to progn style but uses quotes. Does not nest.
;***************************************************************                    
(: (progq) (-> Expression Atom))
(= (progq $exp) 
      (if (== $exp ())
            ()
            (let $IEFBR14 (unquote (car-atom $exp)) (progq (cdr-atom $exp)))))
;



;*******************************************************
; Constants 
;*******************************************************

(highestrank k) 
(highrank q) 
(medrank r) 
(medrank b) 
(medrank n) 
(lowrank p) 

(rank k) 
(rank q) 
(rank r) 
(rank b) 
(rank n) 
(rank p) 

; Chess symbols to display on console
(g k ♔)
(g q ♕)
(g r ♖)
(g b ♗)
(g n ♘) 
(g p ♙)

(s k ♚)
(s q ♛)
(s r ♜) 
(s b ♝)
(s n ♞)
(s p ♟)

;*******************************************************
; Declare initializing game state
;*******************************************************

(game-state initializing)



;*******************************************************
; Code invoked by the basic commands 
;*******************************************************

;***************************************************************
; Function:     add-pieces
; Description:  Converts a board representation into atomspace facts.
;               Each square from the board list becomes a `(square ...)` atom.
;
; Input:        $board - A list of square definitions:
;                        Each square is either (x y) for empty or (x y color piece) if occupied.
;
; Output:       Adds each square to the atomspace as a `(square ...)` atom.
;               Returns True after processing the entire board.
;***************************************************************
(= (add-pieces $board)
  (if (== $board ())
      True
      (let* (
            ($next-square (car-atom $board))
            ($next-square-atom (cons-atom square $next-square))
            ($_ (add-atom &self $next-square-atom))
            )
      (add-pieces (cdr-atom $board)))))

;***************************************************************
; Function:     reset-pieces
; Description:  Clears existing board state from atomspace and
;               re-injects the current board list as square atoms.
;
; Input:        $board - The current board, as a list of square terms:
;                        Each term is (x y) for empty, or (x y color rank) for occupied.
;
; Output:       Atomspace is updated with the board's square atoms.
;***************************************************************
(= (reset-pieces $board)
    (progn
      ; remove prior occupied square atoms from atomspace
      (remove-atom &self (square $x $y $s $p))  
      ; remove empty squares too
      (remove-atom &self (square $x $y))  
      ; now add the present board's squares to atomspace.
      (add-pieces $board)))

;***************************************************************
; Function:     delete-pieces
; Description:  Removes all square-related atoms from the atomspace.
;
; Input:        None
;
; Output:       Atomspace is cleared of all `(square ...)` atoms,
;               both occupied and empty.
;***************************************************************
(= (delete-pieces)
    (progn
      ; remove prior occupied square atoms from atomspace
      (remove-atom &self (square $x $y $s $p))  
      ; remove empty squares too
      (remove-atom &self (square $x $y))  
      ))

;***************************************************************
; Function:     delete-temporary-atoms
; Description:  Removes all temporary `score` atoms used during AI move evaluation.
;
; Input:        None
;
; Output:       Cleans the atomspace of all `(score ...)` atoms,
;               which represent scores of hypothetical evaluations of AI moves.
;***************************************************************
(= (delete-temporary-atoms)
  (match &self
    (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_hypothetical_score)
        ;($X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_hypothetical_score)))
    (remove-atom &self 
    (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_hypothetical_score))))

;***************************************************************
; Function:     delete-prior-game-states
; Description:  Removes any existing `(game-state ...)` atoms from atomspace.
;               Used before setting a new game state.
;
; Input:        None
;
; Output:       Atomspace no longer contains any `game-state` atoms.
;***************************************************************
(= (delete-prior-game-states)
    (collapse (match &self (game-state $prior-state) (remove-atom &self (game-state $prior-state))))
)

;***************************************************************
; Function:     change-game-state
; Description:  Resets the current game state and sets a new one.
;
; Input:        $new-state - A symbol representing the new game state,
;                            e.g., `started`, `checkmate`, `restarted`, etc.
;
; Output:       Replaces any existing `game-state` atom with the new one.
;***************************************************************
(= (change-game-state $new-state)
  (progn
    (delete-prior-game-states) 
    (add-atom &self (game-state $new-state))))

;***************************************************************
; Function:     lots_of_pawns_in_home_row
; Description:  Checks whether more than x AI (gold) pawns are still on row 7.
;               Used to influence early-game move strategy.
;
; Input:        None
;
; Output:       True if more than x (see below) pawns remain on row 7 (initial position);
;               otherwise False.
;***************************************************************
(= (lots_of_pawns_in_home_row)
   (let $pawns_at_home (collapse (match &self (square $x 7 g p) (square $x $y $s $p)))
        (if (> (size-atom $pawns_at_home) 6) ; <-- set here
          True
          False)))

;***************************************************************
; Function:     display_squares_atoms_debug_4
; Description:  Lists all occupied `(square x y color rank)` atoms in atomspace.
;
; Input:        None
;
; Output:       A list of currently occupied squares with full piece info.
;***************************************************************
(= (display_squares_atoms_debug_4)
   (collapse (match &self (square $x $y $s $p) (square $x $y $s $p))))

;***************************************************************
; Function:     display_squares_atoms_debug_2
; Description:  Lists all empty square atoms of form `(square x y)`.
;
; Input:        None
;
; Output:       A list of empty square coordinates in atomspace.
;***************************************************************
(= (display_squares_atoms_debug_2)
    (collapse (match &self (square $x $y) (square $x $y) )))

;***************************************************************
; Function:     display_scores_debug
; Description:  Outputs all `score` atoms used for move evaluation.
;               Each score represents a hypothetical move with its value.
;
; Input:        None
;
; Output:       List of `(score ...)` atoms currently in atomspace.
;***************************************************************
(= (display_scores_debug)
  (collapse (match &self 
    (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_worst_case_score)
    (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_worst_case_score))))

;***************************************************************
; Function:     display-game-state
; Description:  Prints the current `game-state` atom to the console.
;               Helpful for debugging game progress (e.g., "started", "checkmate").
;
; Input:        None
;
; Output:       Prints the current game state.
;***************************************************************
(= (display-game-state)
    (match &self (game-state $msg) 
        (println! $msg)))

;***************************************************************
; Function:     get-player-command
; Description:  Reads characters from user input (console) until <ENTER> is pressed.
;               Used to build command input interactively, one character at a time.
;
; Input:        $input_list - List of previously read characters (initially empty)
;
; Output:       A complete list of characters entered before hitting <ENTER>.
;***************************************************************
(= (get-player-command $input_list)  
   (let $cmd (get-single-char!)
      (progn 
          ; if initial execution flush output
          (if (== (size-atom $input_list) 0) (flush-output!) ())
          (if (== $cmd 13) ; if user hit <ENTER>
            ;return all input
            $input_list    
            ;else gather more input
            (let $new_list (cons-atom $cmd $input_list) (get-player-command $new_list))))))

;***************************************************************
; Function:     welcome
; Description:  Prints the game’s welcome banner and instructions to the console.
;               Also invokes `display_board` to show the current board state.
;
; Input:        None
;
; Output:       Console output displaying the intro text and board layout.
;***************************************************************
(= (welcome)  
 (progn   
    (println! " ") (println! " ") (println! " ") (println! " ")
    (println! "M E T T A    G R E E D Y   C H E S S")
    (println! "This program is a MeTTa exercise which takes the best immediate move without planning far ahead.") 
    ; (display_board (match &self (board-state $board) $board)) 
    (match &self (board-state $board) (println! $board))
    (println! "- Your pieces are white at bottom")
    (println! "*------- C o m m a n d s ------------*")
    (println! "MOVE YOUR PIECE USE example      ->  m <ENTER> a2a3 <ENTER>")
    (println! "   Result:  YOUR pawn in a2 moved to location a3.")
    (println! "Greedy Chess Move (AI)           ->  g")
    (println! "Reset & Replay                   ->  r")
    (println! "Commands List                    ->  c")
    (println! "Display Board                    ->  d")
    (println! "Quit                             ->  q")
    ))

;***************************************************************
; Function:     identify_piece
; Description:  Converts a board square's full data (e.g. `(2 1 s n)`)
;               into small chess symbols for console display.
;
; Input:        $p - A square: either (x y) for empty, or (x y color rank).
;
; Output:       A chess piece console symbol
;***************************************************************
(: (identify_piece) (-> Expression Atom))
(= (identify_piece $p) 
   (if (== (size-atom $p) 2)
      "."
      (let*
        ( ; assign either * or " " 
          ($player (nth 3 $p))
          ; identify piece
          ($piece (nth 4 $p))
          ; look up icon
          ($chess_console_symbol (match &self ($player $piece $symbol) $symbol))
        )
        $chess_console_symbol)))

;***************************************************************
; Function:     identify_piece
; Description:  Converts a board square's full data (e.g. `(2 1 s n)`)
;               into a 2-character string like `*n` (asterisk = human).
;               Empty squares return "  ".
;
; Input:        $p - A square: either (x y) for empty, or (x y color rank).
;
; Output:       A string like "*k", " q", or "  " for rendering the board.
;***************************************************************
(: (identify_piece_text) (-> Expression Atom))
(= (identify_piece_text $p) 
   (if (== (size-atom $p) 2)
      "  "
      (let*
        ( ; assign either * or " " 
          ($contains_bool (contains_symbol $p s))
          ($player (if (== $contains_bool True) * " "))
          ; identify piece
          ($piece (nth 4 $p))
        )
        (format-args "{}{}" ($player $piece)))))

;***************************************************************
; Function:     display_filter
; Description:  Converts entire board list into a list of 2-char piece identifiers
;               using `identify_piece`. Preserves order for rendering.
;
; Input:        $brd - Board list: list of 64 squares.
;
; Output:       A list of symbols like ("*r" "*n" "  " " q" ...)
;***************************************************************
(: (display_filter) (-> Expression Expression))
(= (display_filter $brd) 
    (if (== (size-atom $brd) 1) 
      ; if on last piece, return a one element list of this form eg.:  (*k). Extra parens are needed to create list.
      (let $last_piece (identify_piece (car-atom $brd)) ($last_piece) )
      ; otherwise convert all pieces to shorter description for display.
      (let*
        ( 
        ($rest (display_filter (cdr-atom $brd)))
        ($piece_identified (identify_piece (car-atom $brd)))
        )
        (cons-atom $piece_identified $rest))))

;***************************************************************
; Function:     display_board
; Description:  Pretty-prints the current board to console in a
;               human-readable grid format using identify_piece output.
;
; Input:        $board - Full board as 64-element list of squares
;
; Output:       Formatted string printed to console showing the board with chess symbols.
;***************************************************************
(= (display_board $board) 
  (
   (let* (
          ($pieces (display_filter     $board ))
          )
          (println! (format-args "\n
        -a---b---c---d---e---f---g---h- \n
      8| {} | {} | {} | {} | {} | {} | {} | {} |8\n
      7| {} | {} | {} | {} | {} | {} | {} | {} |7\n
      6| {} | {} | {} | {} | {} | {} | {} | {} |6\n
      5| {} | {} | {} | {} | {} | {} | {} | {} |5\n
      4| {} | {} | {} | {} | {} | {} | {} | {} |4\n
      3| {} | {} | {} | {} | {} | {} | {} | {} |3\n
      2| {} | {} | {} | {} | {} | {} | {} | {} |2\n
      1| {} | {} | {} | {} | {} | {} | {} | {} |1\n
        -a---b---c---d---e---f---g---h- \n
      " 
          $pieces)))))

;***************************************************************
; Function:     display_board_text
; Description:  Pretty-prints the current board to console in a
;               human-readable grid format using identify_piece output.
;
; Input:        $board - Full board as 64-element list of squares
;
; Output:       Formatted string printed to console showing the board.
;***************************************************************
(= (display_board_text $board) 
  (
   (let* (
          ($pieces (display_filter     $board ))
          )
          (println! (format-args "\n
        ---a-------b-------c-------d-------e-------f-------g-------h--- \n
      8|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |8\n
       |---------------------------------------------------------------| \n
      7|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |7\n
       |---------------------------------------------------------------| \n
      6|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |6\n
       |---------------------------------------------------------------| \n
      5|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |5\n
       |---------------------------------------------------------------| \n
      4|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |4\n
       |---------------------------------------------------------------| \n
      3|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |3\n
       |---------------------------------------------------------------| \n
      2|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |2\n
       |---------------------------------------------------------------| \n
      1|  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |  {}   |1\n
        ---a-------b-------c-------d-------e-------f-------g-------h--- \n
      " 
          $pieces)))))

;***************************************************************
; Function:     game-still-playing
; Description:  Determines whether the chess game is still active
;               based on the current `game-state` atom.
;
; Input:        None (uses `match` to query the atomspace)
;
; Output:       Boolean:
;                 - False if the game is in state "checkmate" or "resigned"
;                 - True  otherwise (game still in progress)
;
; Usage:        Called before allowing further commands or moves.
;***************************************************************
(= (game-still-playing) 
  (match &self (game-state $msg)
   (if (or (==  checkmate   $msg) (==  resigned $msg)) 
      False
      True)))

;***************************************************************
; Function:     prompt-for-move
; Description:  Interactively prompts the player to enter a move as
;               a 4-character string (e.g., "a2a3"). Verifies that:
;                 - The input string is exactly 4 characters long
;                 - Each character maps to a valid board coordinate
;
; Input:        None (reads from console using `py-atom input`)
;
; Output:       If valid:
;                  - A pair of coordinate lists:
;                      (x1 y1) - source square
;                      (x2 y2) - destination square
;               If invalid:
;                  - Recursively re-prompts until valid input is given.
;
; Example:      Input "e2e4" ⇒ returns ((5 2) (5 4))
;***************************************************************
(= (prompt-for-move)
  (let*
    (
    ; prompt for x/y location
    ($__1 (println! "Enter valid coordinates."))
    ($coordinates ((py-atom input)))
    ; make sure we have 4 chars, otherwise NIL
    ($input_length (return_length $coordinates))
    ($x1 (if (== $input_length 4)
      (char_to_int ((py-dot $coordinates __getitem__) 0)) NIL))
    ($y1 (if (== $input_length 4)
      (char_to_int ((py-dot $coordinates __getitem__) 1)) NIL))
    ($x2 (if (== $input_length 4)
      (char_to_int ((py-dot $coordinates __getitem__) 2)) NIL))
    ($y2 (if (== $input_length 4)
      (char_to_int ((py-dot $coordinates __getitem__) 3)) NIL))
    ; return NIL if off scale input and redo the prompt
    ($error_check (contains_symbol ($x1 $y1 $x2 $y2) NIL))
    ($f (concat_lists ( ($x1) ($y1))) ) ; Create the source coordinate list.
    ($g (concat_lists ( ($x2) ($y2))) ) ; Create the destination coordinate list.
    )
    ; if invalid coordinates, try again
    (if (== True $error_check)   
      (prompt-for-move)
      ($f $g))))

;***************************************************************
; Function:     decide_greedy_move
; Description:  Core decision-making logic for the AI's move.
;               Attempts multiple strategies:
;               1) Checkmate the human player
;               2) Capture high-value enemy piece
;               3) Threaten a human piece position
;               4) Defend a threatened AI piece
;               5) Make a legal random move
;               6) If everything else fails, make a desperation type of move
;
; Input:        None (relies on current board state via atomspace)
;
; Output:       A move of the form ((x1 y1 g rank) (x2 y2)) or () if no move found.
;***************************************************************
(= (decide_greedy_move) 
    ; make sure each move search call can either 
    ;     1) succeed and return the move, or 
    ;     2) fail and return (empty)
    
      (let* (
        ;($msgd (println! "Greedy Chess deliberating... please wait..."));
        ($msg1 (println! "looking for checkmate..."))
        ($checkmate (collapse 
                      (if (== (lots_of_pawns_in_home_row) True)
                          ; get pieces out at beginning of game
                          (empty)
                          (attemptcheckmate))
                    ))
            )
        (if (not (==  $checkmate ()))
          (car-atom $checkmate)
          
          (let* (
        ($msg2 (println! "looking for capture..."))
        ($highest (collapse (takehighestopen_scored))))
        (if (not (==  $highest ()))
          (car-atom $highest)

          (let* (
        ($msg3 (println! "looking to play defense..."))
        ($position (collapse (playdefense)))
          )
        (if (not (==  $position ()))
          (car-atom $position)

          (let* (
        ($msg4 (println! "looking for threat position..."))
        ($random_threat (random-int  &rng 1 2)) ; get a random number 1 through 2
        ($threaten_position 
                (if (> $random_threat 1)     
                  (collapse (movetoposition))
                  ; return empty parens and skip randomly
                  ()
                  ))
                )
        (if (not (==  $threaten_position ()))
          (car-atom $threaten_position)

        (let* (
        ($msg5 (println! "looking for random move..."))
        ($random_move (collapse (random_move_empty_sq)))
              )
        (if (not (==  $random_move ())) 
          (car-atom $random_move)          

        (let* (
        ($msg6 (println! "looking for desperation move..."))
        ($random_moveD (collapse (random_move_empty_sq_desperate))))
        (if (not (==  $random_moveD ())) 
          (car-atom $random_moveD)  

          ; if everything fails including desperation move, return empty list
          ()
          
          )))))))))))))

;***************************************************************
; Function:     try_move_and_verify
;
; Description:  Evaluates the safety of a potential AI move by:
;                1. Temporarily applying the move to the atomspace
;                2. Checking if the moved piece is immediately exposed
;                3. Checking if the AI king becomes vulnerable as a result
;                4. Optionally scoring the move if it passes verification
;                5. Reverting all changes to leave atomspace unchanged
;
; Input:        ($x1 $y1 g $rank)  - Source square with AI piece
;               ($x2 $y2)         - Target square (occupied or empty)
;               $mode             - If 1, score the move; if 0, skip scoring
;
; Output:       ((from-square) (to-square))  if move is safe and valid
;               (empty)                        if the move is unsafe or invalid
;
; Side Effects: Uses and resets "square" atoms in atomspace for evaluation.
;***************************************************************
; !!---->  So, we just output the input if the move is good
;***************************************************************
(= (try_move_and_verify ($x1 $y1 g $rank)   ($x2 $y2) $mode)
  (let*
    (  
    ($start ($x1 $y1 g $rank))
    ($dest ($x2 $y2))
    ($dest_full (return_entire_box $dest))
    ; provisionally move the piece
    ($MoveBool  (move_piece $start $dest))  
    ; see if moving piece exposes to human player, "s" = silver color
    ($Exposed (take_dest ($x2 $y2) s))
    )
    (if (== $Exposed True)
      ; exit with Empty if the piece can be taken but after moving pieces back in atomspace
      (let $MoveBool  (reset_pieces $start $dest_full) (empty))
      ; else see if king exposed
      (let*
        (
        ; locate AI king
        ($king_square (xy_box (g k)))     
        ($full_king_sq (return_entire_box $king_square))  
        ($KingCompromised (take_dest $full_king_sq s))
        )
        (if (== $KingCompromised True)
            ; exit with Empty if AI king can be taken reversing move in atomspace
            (let $MoveBool  (reset_pieces $start $dest_full) (empty))

            ; SUCCESS! if we make it this far the move is good -- score move and return the input.
            ; still need to reverse move in atomspace since we are testing a lot of moves at once!
            (let* 
              ( 
              ($compute_move_score ; score the move for both players and reset pieces as applicable
                (case $mode
                  (
                    ; write score to Atomspace
                    (1 (score_move ($x1 $y1 g $rank) ($x2 $y2)))
                    ; if 0, don't compute a score
                    (0 0)
                  )))
                ($MoveBool  (reset_pieces $start $dest_full)) 
              )
              (($x1 $y1 g $rank) ($x2 $y2)))) ; return the input move. 
      )
    )
  )
)

;***************************************************************
; Function:     find_by_rank_move_empty_sq
;
; Description:  For a given rank category (e.g., pawn, rook, etc.),
;              this finds AI (gold) pieces that can legally move to
;              any empty square, and verifies that the move is safe.
;
; Input:        $ranklevel — Rank category to search (e.g., lowrank)
;
; Output:       A list of ((from-square) (to-square)) moves
;               or Empty if no safe moves are found.
;
;***************************************************************
(= (find_by_rank_move_empty_sq $ranklevel)
        (match &self 
              (,
             ($ranklevel $rank)
             (square $x1 $y1 g $rank)
             (square $x2 $y2)
              )
                (if (== (clear_route ($x1 $y1 g $rank) ($x2 $y2)) True) 
                  (try_move_and_verify ($x1 $y1 g $rank) ($x2 $y2) 0)
                  (empty)))) 

;***************************************************************
; Function:     return_random_level
;
; Description:  Randomly selects a rank category not yet tried
;              for potential moves. Prioritizes pawns early in game.
;
; Input:        $Ranks_Already_Tried_List — List of rank tags already used
;
; Output:       A new untried ranklevel (lowrank, medrank, etc.), or NIL
;***************************************************************
(= (return_random_level $Ranks_Already_Tried_List)
  (if (== (size-atom $Ranks_Already_Tried_List) 4)
      ; we have tried all 4 rank levels, return NIL
      NIL
      ;else try another random level.
      (let*
        ( 
            ($random_level 
                  ; try to move pawns out from home row early in the game
                  ; *unless* we've already tried pawns (lowrank).
                  (if (and (== (lots_of_pawns_in_home_row) True) 
                           (not (== (intersection-atom $Ranks_Already_Tried_List (lowrank)) (lowrank))))
                  1
                  (random-int  &rng 1 7))) 
            ($rank_level (case $random_level
                        (
                        (1 lowrank)
                        (2 medrank) 
                        (3 highrank)
                        (4 lowrank)
                        (5 medrank) 
                        (6 highrank)
                        (7 highestrank) ;reduce likelihood of king moving for no good reason
                        ($_ lowrank)
                        )))
            
        )
        (if (== ($rank_level) 
                (intersection-atom $Ranks_Already_Tried_List ($rank_level)))
            ; try again if this rank already tried
            (return_random_level $Ranks_Already_Tried_List)
            ; else success, return next new random level.
            $rank_level
        )
      )
  )
)

;***************************************************************
; Function:     random_recursion_by_rank
;
; Description:  Recursively attempts to find a valid AI move by 
;              randomly selecting untried rank categories. If no 
;              moves found for a given rank, that rank is marked 
;              as tried and recursion continues with a new one.
;
; Input:        $Ranks_Already_Tried_List — List of rank tags already tried
;
; Output:       A list of ((from-square) (to-square)) if move found,
;               or Empty if no legal moves exist from any rank group.
;
;***************************************************************
(= (random_recursion_by_rank $Ranks_Already_Tried_List)
    (let* (
      ; Warning: as of this writing random-int's max value is 1 more than the actual max.
      ;    If random-int is changed to return beyond the max, the case statement will return lowrank.
      ($ranklevel (return_random_level $Ranks_Already_Tried_List))
        )
      (if (== $ranklevel NIL)
        ; if we have exhausted all possible moves by level return empty list
        (empty)
        ; else try a move by level
        (let $somemoves   (collapse (find_by_rank_move_empty_sq $ranklevel))
          (if (== $somemoves ())
            ; if no move found
            (random_recursion_by_rank (cons-atom $ranklevel $Ranks_Already_Tried_List))
            ; Success! return move
            $somemoves)))))

;***************************************************************
; Function:     random_move_empty_sq
;
; Description:  Selects and returns one random legal AI move to an 
;               empty square. All moves are generated by randomly 
;               choosing from piece ranks and validated for legality.
;               The move is only "envisioned" (i.e., atomspace is updated
;               temporarily) and then restored.
;
; Input:        None
;
; Output:       A single move of form ((x1 y1 g rank) (x2 y2)), or Empty
;
; Notes:        - Uses helper: random_recursion_by_rank
;               - Respects MeTTa's atomspace non-destructive move philosophy
;               - Handles quirks of random-int range
;
;***************************************************************
(= (random_move_empty_sq)
  (let $random_moves_collapsed (collapse (random_recursion_by_rank ()))
    (if (== $random_moves_collapsed ())
        (empty)
        (let*
          (
          ($random_moves (car-atom $random_moves_collapsed)) ;get rid of an extra set of parens
          ($move_count (size-atom $random_moves))
          ($select_int (if (== $move_count 2) 
                    1
                    ; use &rng after random-int!
                    (random-int  &rng 1 $move_count))) 
          ($select_int_check (if (> $select_int $move_count) 1 $select_int)) ; if problem with random-int
          ) 
          ;return move
          (nth $select_int_check $random_moves)
          ))))
              
;***********************************************************************
; Function:     random_move_empty_sq_desperate
;
; Description:  Attempts to find *any* legal AI (gold) move that would 
;               escape from checkmate. It is invoked as a last resort 
;               when standard move generation fails.
;
; Input:        None
;
; Output:       A single valid move in the form ((x1 y1 g rank) (x2 y2)), 
;               if one exists; otherwise returns Empty.
;
; Side Effects: - Relies on transient square atoms in atomspace.
;               - Uses `any_moves_to_escape` to probe escape moves.
;
; Note:         This function acts as a fallback if the AI is under 
;               immediate threat of checkmate and no optimal moves 
;               can be found through standard AI strategies.
;***********************************************************************
(= (random_move_empty_sq_desperate)
    (let $any_moves_left_whatsoever (collapse (any_moves_to_escape g))
      (if (not (== $any_moves_left_whatsoever ())) ;an empty list means no moves for escape exist, checkmate!
            (car-atom $any_moves_left_whatsoever)  ; "Custer's last stand" type of move is possible
            (empty))))

;***********************************************************************
; Function:     attemptcheckmate
;
; Description:  Attempts to detect and execute a checkmate move against 
;               the human (silver) player by simulating all gold piece 
;               attacks. Each possible move is tested to see if it leaves
;               the human king with no valid escapes.
;
; Input:        None
;
; Output:       If a checkmate is found, returns the move as 
;               ((x1 y1 g rank) (x2 y2)); otherwise returns Empty.
;
; Side Effects: - Updates game-state atom to (checkmate) if checkmate found.
;               - Moves are only provisionally made in atomspace.
;               - State is always reverted after testing each move.
;
; Note:         Relies on `attemptcheckmate_match`, `move_piece`, 
;               `any_moves_to_escape`, and `reset_pieces`.
;***********************************************************************
(= (attemptcheckmate) 
  (let $checkmate_moves (collapse (attemptcheckmate_match))
    ;(if (== $checkmate_moves ()) (empty) (car-atom $checkmate_moves))))  <-- works alone for check!

    ; the following block of code determines if, after a check move, the king can escape by any means (block etc)
    (let $list_of_checkmate_moves 
        (collapse 
          (let $test_move (superpose $checkmate_moves)
            (let*
              ( 
                ($start (nth 1 $test_move))
                ($dest (nth 2 $test_move))
                ($dest_full (return_entire_box $dest))
                ; provisionally move the piece
                ($__1  (move_piece $start $dest))  
                ; with move envisioned, see if the silver king can escape using any moves whatsoever
                ($any_moves_left_whatsoever (collapse (any_moves_to_escape s)))
                ; make sure gold AI king safe
                ($king_status (checkking g))
                ($__2 (reset_pieces $start $dest_full))
              )
              (if (and (== $any_moves_left_whatsoever ()) ;an empty list means no moves for escape exist.
                       (== $king_status False))
                    ; if no moves possible for human, Greedy Chess has found checkmate, success! 
                    $test_move  
                    (empty)))))
        (if (== $list_of_checkmate_moves ())
          (empty)
          (progn 
            (change-game-state checkmate)
            (car-atom $list_of_checkmate_moves))
        )
    )
  )
)

;***********************************************************************
; Function:     attemptcheckmate_match
;
; Description:  Generates candidate AI moves that may result in checkmate 
;               by finding a clear route from an AI piece to an empty square,
;               and from that square to the human (silver) king.
;
; Input:        None (operates on Atomspace state)
;
; Output:       List of possible checkmate setup moves of the form:
;               ((x1 y1 g rank) (x2 y2)), where (x1 y1) is the current 
;               location of the AI piece and (x2 y2) is the target square.
;               Returns Empty if no such move is found.
;
; Dependencies: - Requires `xy_box` to locate king.
;               - Uses `clear_route` to test movement paths.
;
; Side Effects: None. Purely generates candidate moves.
;***********************************************************************
(= (attemptcheckmate_match) 
  ; can an AI piece put human king in check?
  ; locate silver king (human player's king)
  (let $king_square (xy_box (s k))
    ; find all AI pieces
    (match &self (square $x1 $y1 g $AI_rank)
      ; find empty squares
      (match &self (square $x2 $y2)
      ; look for check route to king
        (if (and (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True)
                 (== (clear_route ($x2 $y2 g $AI_rank) $king_square) True))
          ; king in check
          (($x1 $y1 g $AI_rank) ($x2 $y2))
          ; else fail
          (empty))))))

;*************************************************************************
; Function:     attemptcheckmate_match
;
; Description:  Searches for AI (gold) moves that attack an occupied square 
;               (i.e., one with a human/silver piece), and from that position 
;               can check the silver king. Used to simulate aggressive AI 
;               threats.
;
; Input:        None (operates over current Atomspace state)
;
; Output:       List of possible checkmate-setup moves of the form:
;               ((x1 y1 g rank) (x2 y2)) where the destination is an
;               occupied square containing a human player's piece.
;               Returns Empty if no valid candidate is found.
;
; Dependencies: - Requires `xy_box` to find the king.
;               - Uses `clear_route` to check movement paths.
;
; Notes:        This variant assumes AI can directly attack human-occupied
;               squares, rather than only moving to empty ones.
;*************************************************************************
(= (attemptcheckmate_match) 
  ; can an AI piece put human king in check?
  ; locate silver king (human player's king)
  (let $king_square (xy_box (s k))
    ; find all AI pieces
    (match &self (square $x1 $y1 g $AI_rank)
      ; find occupied squares
      (match &self (square $x2 $y2 s $anyrank)
      ; look for check route to king
        (if (and (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True)
                 (== (clear_route ($x2 $y2 g $AI_rank) $king_square) True))
          ; king in check
          (($x1 $y1 g $AI_rank) ($x2 $y2))
          ; else fail
          (empty))))))

;*************************************************************************
; Function:     movetoposition
;
; Description:  Searches for AI (gold) pieces that can move to an empty 
;               square such that, after the move, they threaten a valuable 
;               human (silver) piece (e.g., king, queen, rook, knight, bishop).
;
; Input:        None (relies on Atomspace state)
;
; Output:       One randomly selected move ( (start) (destination) ) that 
;               puts a human piece at risk; or `empty` if none found.
;
; Subfunction:  movetoposition_match
;
; Note:         Uses a random selection over all available threat moves to 
;               create unpredictability.
;*************************************************************************
(= (movetoposition)
  (let $threaten_moves_all 
    ; find all the possible threatening moves the AI can take
    (collapse (movetoposition_match (superpose (q r n b)) (superpose (k q r n b))))
    (if (== $threaten_moves_all ())
      (empty)
      (let*
        (
          ; randomize the threat move
        ($size_move (size-atom $threaten_moves_all))
        ;($size_max (+ $size_move 1))
        ($random_select (random-int  &rng 1 $size_move))
        ($move (nth $random_select $threaten_moves_all))
        )
        $move))))

;*************************************************************************
; Function:     movetoposition_match
;
; Description:  Finds all possible positions an AI piece can move to that 
;               would place it in line to attack a human piece in the next move.
;
; Input:        
;   - $AI_rank      : A gold (AI) piece type to move (e.g., q, r, n, b)
;   - $targetrank   : A target human piece type to threaten (e.g., k, q, r, n, b)
;
; Output:       
;   - List of potential moves: ( (start) (intermediate empty square) )
;   - Only returns moves where the AI piece is *not* exposed after the move.
;
; Logic:
;   1. Find AI piece of the given rank.
;   2. Check if it can move to any empty square.
;   3. From that new square, check if it threatens a human piece of $targetrank.
;   4. Only retain moves that don't expose the AI piece to immediate capture.
;
; Safety: Provisional move simulation and rollback ensure Atomspace state integrity.
;*************************************************************************
(= (movetoposition_match $AI_rank $targetrank ) 
    ; match all gold pieces (AI)
    (match &self (square $x1 $y1 g $AI_rank)
      ; match all empty squares
     (match &self (square $x2 $y2)
        ; if the gold piece has a route to an empty square
        (if (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True)
            ; now find all target human pieces available
            (match &self (square $x3 $y3 s $targetrank)
              ; if there is a route to target from new square
              (if (== (clear_route ($x2 $y2 g $AI_rank) ($x3 $y3)) True)
                    ; if AI piece is not exposed in new position
                    (let*
                          ( ;envision the move to empty square
                            ($__1 (move_piece ($x1 $y1 g $AI_rank) ($x2 $y2)))
                            ; see if exposed after moves
                            ($exposed (take_dest ($x2 $y2) s))
                            ; make sure king safe
                            ($king_status (checkking g))
                            ($__2 (reset_pieces ($x1 $y1 g $AI_rank) ($x2 $y2)))
                          )
                          (if (and (== $exposed False) (== $king_status False))
                            ; success!
                            (($x1 $y1 g $AI_rank) ($x2 $y2))
                            ; whoops, the piece is exposed to capture
                            (empty)))
                    (empty)
              )
            )
          (empty)
        ))))

;*************************************************************************
; Function:     playdefense
;
; Description:  Attempts to save AI (gold) pieces currently under threat
;               from a human (silver) piece by finding a safe square to flee to.
;
; Input:        None (relies on current atomspace state)
;
; Output:       A random safe move for an endangered piece 
;               ( (start) (destination) ), or `empty` if none found.
;
; Subfunction:  playdefense_match
;
; Notes:        
;   - Currently prioritizes major pieces: (q, r, n, b)
;   - Pawn defense is not included but could be added.
;*************************************************************************
(= (playdefense)
  (let $defense_all
    (collapse (playdefense_match (superpose (k q r n b))))
    (if (== $defense_all ())
      (empty)
      (let*
        (
          ; randomize the move
        ($size_move (size-atom $defense_all))
        ;($size_max (+ $size_move 1))
        ($random_select (random-int  &rng 1 $size_move))
        ($move (nth $random_select $defense_all))
        )
        $move))))

;*************************************************************************
; Function:     playdefense_match
;
; Description:  Matches AI pieces of the given rank that are threatened 
;               and attempts to find a safe, unoccupied square to retreat to.
;
; Input:        
;   - $AI_rank: The type of gold (AI) piece 
;
; Output:       
;   - A list of viable escape moves, where the AI piece:
;       1. Is currently under threat (`take_dest` returns True),
;       2. Can legally move to an empty square (`clear_route` returns True),
;       3. Will *not* be threatened in the new square after the move.
;
; Safety: 
;   - Moves are provisionally applied and then reverted (via `reset_pieces`)
;     to maintain Atomspace integrity during simulations.
;*************************************************************************
(= (playdefense_match $AI_rank) 
    ; find all AI pieces
    (match &self (square $x1 $y1 g $AI_rank)
        ; if piece under threat by human
        (if (== (take_dest ($x1 $y1) s) True)
          ; if square not covered by a different AI piece that could in turn capture attacking human piece,
          (if (or (== (take_dest ($x1 $y1) g) False)
                   ;   except if threatened piece is a queen. If queen is threatened play defense regardless...
                   (and (== (take_dest ($x1 $y1) g) True) (== $AI_rank q)))
            ; find all empty squares to flee to...
            (match &self (square $x2 $y2)
                ; if piece can escape to an empty square
                (if (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True)
                    ; if AI piece is not exposed in new position
                    (let*
                          ( ;envision the move to empty square, see if exposed after moves
                            ($__1 (move_piece ($x1 $y1 g $AI_rank) ($x2 $y2)))
                            ($exposed (take_dest ($x2 $y2) s))
                            ; make sure king safe
                            ($king_status (checkking g))
                            ($__2 (reset_pieces ($x1 $y1 g $AI_rank) ($x2 $y2)))
                          )
                          (if (and (== $exposed False) (== $king_status False))
                            ; success!
                            (($x1 $y1 g $AI_rank) ($x2 $y2))
                            ; whoops, the piece is exposed to capture
                            (empty))
                    )
                    (empty)
                )
            )
            (empty)
          )
          (empty)
        )
    )
)

; This variation will play defense if high ranking pieces are ever threatened by a pawn.
(= (playdefense_match $AI_rank) 
    ; find all AI pieces
    (match &self (square $x1 $y1 g $AI_rank)
        ; find human pawns...
        (match &self (square $PAWNx1 $PAWNy1 s p)
          ; if there is a capture route for the human pawn to capture higher rank AI piece
          (if (== (clear_route ($PAWNx1 $PAWNy1 s p) ($x1 $y1)) True) 
            ; find all empty squares to flee to...
            (match &self (square $x2 $y2)
                ; if piece can escape to an empty square
                (if (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True)
                    ; if AI piece is not exposed in new position
                    (let*
                          ( ;envision the move to empty square, see if exposed after moves
                            ($__1 (move_piece ($x1 $y1 g $AI_rank) ($x2 $y2)))
                            ($exposed (take_dest ($x2 $y2) s))
                            ; make sure king safe
                            ($king_status (checkking g))
                            ($__2 (reset_pieces ($x1 $y1 g $AI_rank) ($x2 $y2)))
                          )
                          (if (and (== $exposed False) (== $king_status False))
                            ; success!
                            (($x1 $y1 g $AI_rank) ($x2 $y2))
                            ; whoops, the piece is exposed to capture
                            (empty)))
                    (empty)
                )
            )
            (empty)))))

;*************************************************************************
; Function:     takehighestopen_scored
;
; Description:  Attempts to capture a human (silver) piece with a gold
;               (AI) piece, choosing the move that minimizes a composite
;               heuristic score from both perspectives.
;
; Input:        None
; Output:       Best AI capture move as ((start) (destination)), or `empty`
;
; Internals:
;   - Calls `takehighestopen` to find candidate capture moves.
;   - Scores each via `score_move`, then uses `finalize_all_scored_moves`
;     and `find_best_score` to choose the optimal path.
;
; Cleanup:      Deletes temporary `(score ...)` atoms after evaluation.
;*************************************************************************
(= (takehighestopen_scored)
    (let $highest_open (collapse (takehighestopen (superpose (b r n q p))))
      (if (not (== $highest_open ()))
        (let*
            ( 
            ($all_final_scored_moves (collapse (finalize_all_scored_moves)))            
            ; find best (lowest) score
            ($best_move_final_winner (find_best_score $all_final_scored_moves))
            ($best_move (cdr-atom $best_move_final_winner))
            )
            ; clean up score atoms and return optimal move
            ; Retain the usual "square" atoms used to envision the possible moves.
            (let $temp_delete (collapse (delete-temporary-atoms))
                  $best_move)
        )
        ; else no candidate moves
        (empty))))
    
;*************************************************************************
; Function:     finalize_all_scored_moves
;
; Description:  Reformats `(score ...)` atoms to tuple form:
;               (final_score (start) (destination))
;
; Output:       List of scored moves to be compared for optimality.
;*************************************************************************
(= (finalize_all_scored_moves)
      (match &self 
        (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_hypothetical_score)            
        (let  
          $final_score (+ $Opponent_hypothetical_score $AI_hypothetical_score)
            ; return final score with move
            ($final_score ($X1 $Y1 $color $rank) ($X2 $Y2)))))

;*************************************************************************
; Function:     find_best_score
;
; Description:  Recursively evaluates a list of scored move tuples, 
;               returning the move with the lowest total score.
;
; Input:        $scored_moves: List of ((score) (start) (dest)) triples
; Output:       Best scoring triple (lowest score)
;
; Assumes:      List is not empty.
;*************************************************************************
(= (find_best_score $scored_moves)
   (if (== (size-atom $scored_moves) 1)
        (car-atom $scored_moves)
        ; at least two moves left in list... don't call this with empty list ()
        (let*
          (
          ($first_move (car-atom $scored_moves))
          ($first_score_int (car-atom $first_move))
          ($rest_moves (cdr-atom $scored_moves))
          ($second_move (car-atom $rest_moves))
          ($second_score_int (car-atom $second_move))
          )
          ; if the first score in list less than second score in list, return lowest of first and 
          ;     all moves left after second if any
          ; if the first score is greater than the second score in the list, return lowest of
          ;     second move and all moves left.
          (if (or (< $first_score_int $second_score_int) (== $first_score_int $second_score_int))
              (let*     (
                    ($any_remaining_moves (cdr-atom $rest_moves))
                    ($remainder (cons-atom $first_move $any_remaining_moves))
                        )
                    (find_best_score $remainder))
              (find_best_score $rest_moves)))))

; pick from lowest combined score from the scored atoms
; use a single match for each score atom. Create a 'winner' atom. If a match has
; lower combined score update the 'winner' then return the winner to above call.
; if no winner return (empty)


;*************************************************************************
; Function:     takehighestopen
;
; Description:  Computes all **legal AI capture moves** targeting opponent
;               pieces of a given rank (`$Opponent_rank`), filtered for
;               safety using `try_move_and_verify`.
;
; Input:        $Opponent_rank: The rank symbol of the opponent piece to target (e.g., q, r, b, n, p)
; Output:       List of valid and safe capture moves of the form:
;                 ((x1 y1 g rank) (x2 y2))
;
; Notes:
;   - This function is defined in **two overloaded clauses**:
;
;      **Pawn Variant:**
;         - Restricts gold pawn movement to diagonal forward (southwest/southeast).
;         - Conditions:
;             y2 = y1 - 1
;             x2 = x1 ± 1
;         - Does **not** use `clear_route`.
;
;      **Non-Pawn Variant:**
;         - For queens, rooks, bishops, knights.
;         - Uses `clear_route` to verify valid path.
;         - Skips pawns via `(not (== $AI_rank p))`.
;
; Shared:
;   - Both clauses use `try_move_and_verify` with mode = 1
;     (to record a temporary `(score ...)` atom for evaluation).
;*************************************************************************
; if pawns can capture, return all possibilities
(= (takehighestopen $Opponent_rank)
    ; match all opponent occupied squares
    (match &self (square $x2 $y2 s $Opponent_rank)
       ; match all AI occupied squares
       (match &self (square $x1 $y1 g p) 
          ; the gold pawn always has a clear route to capture if moving southwest or southeast.
          (if (and (== $y2 (- $y1 1)) (or (== $x2 (- $x1 1)) (== $x2 (+ $x1 1)))) 
            (try_move_and_verify ($x1 $y1 g p) ($x2 $y2) 1)
            (empty)))))
; if higher ranking (non pawns) can capture, return all possibilities
(= (takehighestopen $Opponent_rank)
    ; match all opponent occupied squares
    (match &self (square $x2 $y2 s $Opponent_rank)
       ; match all AI occupied squares
       (match &self (square $x1 $y1 g $AI_rank) 
          ; check all non-pawn pieces for attack
          (if (and (== (clear_route ($x1 $y1 g $AI_rank) ($x2 $y2)) True) (not (== $AI_rank p))) 
            (try_move_and_verify ($x1 $y1 g $AI_rank) ($x2 $y2) 1)
            (empty)))))

;obsolete for comparison only
(= (take_dest_recursive $Square $OpponentColor $Board) 
  (let* 
    (
    ;  Identify opponent pieces that can attack.
    ($CanAttack (takingboxes_recursive $OpponentColor $Board))
    ; Check if any route of pieces that can attack leads to the specified square.
    ($OpenRouteToSquare (list_clear_route $Board $Square $CanAttack))
    )
    ; if the list is populated (size exceeds 0), that means the piece can be taken.
    (if (not (== $OpenRouteToSquare () )) True False)))
    
;***********************************************************************
; Function:    take_dest
; Description: Check if a square is threatened by opponent's pieces.
; Input:       $Square         – e.g. (3 3)
;              $OpponentColor  – e.g. s or g
; Output:      True  if any opponent piece can attack the square
;              False otherwise
;***********************************************************************
(= (take_dest $Square $OpponentColor)
    (let*
        (
        ;  Identify opponent pieces that can attack.
        ($CanAttack (collapse (takingboxes $OpponentColor)))
        ; Check if any route of pieces that can attack leads to the specified square.
        ($OpenRouteToSquare (collapse (list_clear_route $Square $CanAttack)))
        )
        ; if the list is populated (size exceeds 0), that means the piece can be taken, return False
        (if (== $OpenRouteToSquare ()) False True)))

;***********************************************************************
; Function:    return_entire_box
; Description: Retrieve the full square data from coordinates.
; Input:       Coordinates (x y) or full square (x y c d)
; Output:      A full square: either (x y) or (x y c d)
;***********************************************************************
(= (return_entire_box ($x $y))
    (match &self (square $x $y) ($x $y)))
(= (return_entire_box ($x $y))
    (match &self (square $x $y $c $d) ($x $y $c $d)))
(= (return_entire_box ($x $y $c $d)) ($x $y $c $d))

;***********************************************************************
;   return_entire_box_sequential
;
;   Retrieve the full details of a square on the board based on its coordinates
;   using a sequential search.
;   
;   Input:
;   $Coordinates The coordinates of the desired square.
;   $Board The current board configuration.
;
;   Returns:
;   $next-square The full contents of square.
;***********************************************************************
(= (return_entire_box_sequential $Coordinates $Board) 
  (if (== (size-atom $Coordinates) 4) 
   ; if you send this the full box just output the input, no need for further evaluation.
    $Coordinates
   ;else
    (let*
    ;examine the next square on the board
    (($next-square (car-atom $Board)  )
    ($X (nth 1 $next-square) )
    ($Y (nth 2 $next-square) )
    ;($G (concat_lists (($X) ($Y))) )) 
    )
    ; then execute
    (if (== ($X $Y) $Coordinates) 
      $next-square
      (return_entire_box_sequential $Coordinates (cdr-atom $Board))))))

;***********************************************************************
; Function:    xy_box
; Description: Find the coordinates of a piece on the board.
; Input:       A piece descriptor in the form: (color rank)
; Output:      Coordinates (x y) where the piece is located
;***********************************************************************
(= (xy_box ($PieceColor $PieceRank))
       (match &self (square $x $y $PieceColor $PieceRank) ($x $y))) 

;***********************************************************************
; obsolete comparison versions...
;
; (xy_box_recursive)
;     1. Searches the board for a square containing the specified piece.
;     2. Returns the x/y coordinates of the square if a match is found.
;
; NOTE: This version of xy_box is only designed to return 1 piece deterministically 
;       given 1 piece of specified type exists.
;       For multiple pieces and backtracking action devise another means.
;***********************************************************************
(= (xy_box_recursive ($PieceColor $PieceRank) $Board)
  (let*
    (
    ; assign variables for the next piece
    ;($debug (println! $PieceColor))
    ($next_square (car-atom $Board)  )
    ($next_x (nth 1 $next_square) )
    ($next_y (nth 2 $next_square) )
    )
    ; then examine next square
    (if (== (size-atom $next_square) 2) 
      ; empty square, skip
      (xy_box_recursive ($PieceColor $PieceRank) (cdr-atom $Board))
      ; else check this piece
      (let* (
            ($next_color (nth 3 $next_square) )
            ($next_rank  (nth 4 $next_square) )
            )
            (if
              (and (== $next_color $PieceColor)
                   (== $next_rank $PieceRank))
                ; return x and y of the found piece
                ($next_x $next_y)
                ; else keep checking the board
                (xy_box_recursive ($PieceColor $PieceRank) (cdr-atom $Board)))))))
  
;(: (clear_route) (-> Expression Expression  Bool))

;*******************************************************************************
; Function:    clear_route
; Description: Validates whether a specific chess piece can legally move from a 
;              source square to a destination square, based on the rules of 
;              movement for each piece type. This function ensures that:
;              - The move adheres to each piece's geometry (e.g., bishop diagonals, rook lines).
;              - All intermediate squares (for sliding pieces) are unoccupied.
;              - Pawn-specific rules (direction, captures, initial two-step) are honored.
;
; Input:       Two expressions:
;              1. A piece located at a specific coordinate in the form: (x y color rank)
;              2. A destination square in the form: (x y)
;
; Output:      Boolean (True if the route is legal and clear; otherwise False)
;
; Notes:       
;   * Before calling this function, ensure that the source and destination are distinct,
;     and that if the destination is occupied, it contains an opponent’s piece.
;   * This function handles routing logic only. Occupancy checks must be performed externally.
;   * Supports: king, queen, rook, bishop, knight, pawn (both silver and gold variants).
;
; Dependencies:
;   * return_entire_box — fetches the full content of a square.
;   * clearcheck[dir]   — used for recursive visibility checks in linear or diagonal paths.
;
; Piece Logic Overview:
;   * King     — One square in any direction.
;   * Queen    — Combines rook and bishop logic.
;   * Rook     — Straight lines (vertical/horizontal), path must be clear.
;   * Bishop   — Diagonals, path must be clear.
;   * Knight   — "L" shaped jumps (does not consider blocking).
;   * Pawn     — Moves differ by color (gold vs. silver), includes captures, forward moves, 
;                and initial 2-step logic.
;*******************************************************************************

; Return True if there is a clear route for piece else False. The first parm is starting square, 2nd parm destination, returns BOOL.
; Note: prior to calling clear_route verify that the piece is moving to a different square and
; if the square is occupied it is occupied by the opponent's color. These tests are not duplicated by clear_route!

; TEST ONLY, uncomment to get success/fail as needed

;(= (clear_route ($X1 $Y1 $COLOR p) $destination) True)
;(= (clear_route ($X1 $Y1 $COLOR r) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR n) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR b) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR q) $destination) False)
;(= (clear_route ($X1 $Y1 $COLOR k) $destination) False)

; King: Moves one square in any direction.
(= (clear_route ($X1 $Y1 $Color k) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )          
             )
      (if (and 
         (or      
          (== $X2 $X1)
          (or 
          (== $X2 (- $X1 1))
          (== $X2 (+ $X1 1))
          )) 
         (or
          (== $Y2 $Y1)
          (or 
          (== $Y2 (- $Y1 1))
          (== $Y2 (+ $Y1 1))
          )) 
          )
        True
        False)))

; Queen:  Either rook or bishop move works.
(= (clear_route ($X1 $Y1 $Color q) $destination)
      (if (or (clear_route ($X1 $Y1 $Color b) $destination)
               (clear_route ($X1 $Y1 $Color r) $destination))
           True
           False))

; Knight: Moves in an "L" shape (2 steps in one direction, 1 step perpendicular).
(= (clear_route ($X1 $Y1 $Color n) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )          
             )
      (if (or 
          ; vertical 'L'
         (and
          (or 
          (== $X2 (- $X1 1))
          (== $X2 (+ $X1 1))
          )
          (or
          (== $Y2 (- $Y1 2))
          (== $Y2 (+ $Y1 2))
          )
         ) 
          ; sideways 'L'
         (and
          (or
          (== $X2 (- $X1 2))
          (== $X2 (+ $X1 2))
          )
          (or
          (== $Y2 (- $Y1 1))
          (== $Y2 (+ $Y1 1))
          )
          ))
        True
        False)))

; Move gold AI pawn 
(= (clear_route ($X1 $Y1 g p) $destination)
  (let*
    ( 
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )
      ($entire (return_entire_box $destination))
      ($passing_1 ($X2 6) )
      ($entire_box_down_one  (return_entire_box $passing_1))
      )
    
    (if (or 
        
          ; clear if moving to empty box 1 row down...
          (and  
          (== $X1 $X2)
          (and 
          (== $Y2 (- $Y1  1))
          ; check empty box to move into
          (== (size-atom $entire) 2)
          ))

          (or
            ; clear if moving to empty box 2 rows down from staring position
            (and 
            ; moving down in same column  
            (== $X1 $X2)
            (and 
            ; moving from row 7
            (== $Y1 7)
            (and 
            ; definitely moving down 2 
            (== $Y2 5)
            (and 
            ; destination box down 2 is clear
            (== (size-atom $entire) 2)       
            ; first box down 1 is clear
            (== (size-atom $entire_box_down_one) 2))
            )))
    
            (or
              ; clear if taking piece one box down and to the right
              (and  
              (== $X2 (+ $X1 1))
              (and 
              (== $Y2 (- $Y1 1))
              ; make sure there is a piece to take, must have 4 symbols in the target box
              (== (size-atom $entire) 4)
              )) 
      
              ; clear if taking piece one box down and to the left
              (and  
              (== $X2 (- $X1 1))
              (and 
              (== $Y2 (- $Y1 1))
              ; make sure there is a piece to take, must have 4 symbols in the target box
              (== (size-atom $entire) 4)
              ))
            )
          )
        )
        True
        False)))

; Move silver human pawn 
(= (clear_route ($X1 $Y1 s p) $destination)
  (let*
    ( 
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )      
      ($entire (return_entire_box $destination))
      ($passing_1 ($X1 3) )
      ($entire_box_up_one  (return_entire_box $passing_1))
      )
    
    (if (or 
         (or      
          ; clear if taking piece one box up and to the right
          (and  
          (== $X2 (+ $X1 1))
          (and 
          (== $Y2 (+ $Y1 1))
          ; make sure there is a piece to take, must have 4 symbols in the target box
          (== (size-atom $entire) 4)
          )) 
  
          ; clear if taking piece one box up and to the left
          (and  
          (== $X2 (- $X1 1))
          (and 
          (== $Y2 (+ $Y1 1))
          ; make sure there is a piece to take, must have 4 symbols in the target box
          (== (size-atom $entire) 4)
          ))
         ) 
         (or
          ; clear if moving to empty box 1 row up
          (and  
          (== $X1 $X2)
          (and 
          (== $Y2 (+ $Y1  1))
          ; check empty box to move in to
          (== (size-atom $entire) 2)
          ))

          ; clear if moving to empty box 2 rows up from staring position
          (and 
          ; moving up in same column  
          (== $X1 $X2)
          (and 
          ; moving from row 2
          (== $Y1 2)
          (and 
          ; definitely moving up 2 
          (== $Y2 4)
          (and 
          ; destination box up 2 is clear
          (== (size-atom $entire) 2)       
          ; first box up 1 is clear
          (== (size-atom $entire_box_up_one) 2))
          )))
         )
        )
        True
        False)))

; Rook: Moves horizontally or vertically.
(= (clear_route ($X1 $Y1 $Color r) $destination)
      (let*  (   
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )    
      ($RouteClearBool 
           ; if up
           (if (and (== $X2 $X1) (> $Y2  $Y1))
               (clearcheckup $X1 (+ 1 $Y1) $Y2) 
           ;elif down
           (if (and (== $X2 $X1) (< $Y2  $Y1))
               (clearcheckdown $X1 (- $Y1 1) $Y2)
           ;elif right
           (if (and (> $X2 $X1) (== $Y2  $Y1))
                (clearcheckright (+ $X1 1) $X2 $Y1)
           ;elif left
           (if (and (< $X2 $X1) (== $Y2  $Y1))
               (clearcheckleft (- $X1 1) $X2 $Y1) 
            ; otherwise rook bad move
               False)))))
      )
      $RouteClearBool))

;(: clearcheckup (-> Atom Atom Atom Bool))
;(: clearcheckdown (-> Atom Atom Atom Bool))
;(: clearcheckleft (-> Atom Atom Atom Bool))
;(: clearcheckright (-> Atom Atom Atom Bool))

; These helper routines for the rook move work by testing for empty squares for the rook to pass to its destination.
; Sanity checks are included so we don't exceed the space on the board, and if so, return False.
(= (clearcheckup $X $Y1 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (< $Y1 9) 
          ;return all the contents of the square
          (return_entire_box ($X $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (== $Y1 $Y2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckup $X (+ $Y1 1) $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckdown $X $Y1 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if  (> $Y1 0)
          ;return all the contents of the square
          (return_entire_box ($X $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (== $Y1 $Y2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckdown $X (- $Y1 1) $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckright $X1 $X2 $Y) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if  (< $X1 9) 
          ;return all the contents of the square
          (return_entire_box ($X1 $Y)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (== $X1 $X2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckright (+ $X1 1) $X2 $Y)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckleft $X1 $X2 $Y) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (> $X1 0)
          ;return all the contents of the square
          (return_entire_box ($X1 $Y)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (== $X1 $X2) 
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckleft (- $X1 1) $X2 $Y)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

; Bishop: Moves diagonally
(= (clear_route ($X1 $Y1 $Color b) $destination)
      (let*  (
      ($X2 (nth 1 $destination) )  
      ($Y2 (nth 2 $destination) )    
      ($RouteClearBool 
           ; if northeast
           (if (and (> $X2 $X1) (> $Y2  $Y1))
               (clearcheckNE (+ 1 $X1) (+ 1 $Y1) $X2 $Y2) 
           ;elif southeast
           (if (and (> $X2 $X1) (< $Y2  $Y1))
               (clearcheckSE (+ 1 $X1) (- $Y1 1) $X2 $Y2)
           ;elif northwest
           (if (and (< $X2 $X1) (> $Y2  $Y1))
               (clearcheckNW (- $X1 1) (+ $Y1 1) $X2 $Y2)
           ;elif southwest
           (if (and (< $X2 $X1) (< $Y2  $Y1))
               (clearcheckSW (- $X1 1) (- $Y1 1) $X2 $Y2)
            ; otherwise bishop bad move
               False)))))
      )
      $RouteClearBool))

;(: clearcheckNE (-> Atom Atom Atom Atom Bool))
;(: clearcheckSE (-> Atom Atom Atom Atom Bool))
;(: clearcheckNW (-> Atom Atom Atom Atom Bool))
;(: clearcheckSW (-> Atom Atom Atom Atom Bool))

; These helper routines for the bishop move work by testing for empty squares for the bishop to pass to its destination.
; Sanity checks are included so we don't exceed the space on the board, and if so, return False.
(= (clearcheckNE $X1 $Y1 $X2 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (and (and (< $X1 9) (> $X1 0)) (and (< $Y1 9) (> $Y1 0))) 
          ;return all the contents of the square
          (return_entire_box ($X1 $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckNE (+ $X1 1) (+ $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckSE $X1 $Y1 $X2 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (and (and (< $X1 9) (> $X1 0)) (and (< $Y1 9) (> $Y1 0))) 
          ;return all the contents of the square
          (return_entire_box ($X1 $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckSE (+ $X1 1) (- $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckNW $X1 $Y1 $X2 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (and (and (< $X1 9) (> $X1 0)) (and (< $Y1 9) (> $Y1 0))) 
          ;return all the contents of the square
          (return_entire_box ($X1 $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckNW (- $X1 1) (+ $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

(= (clearcheckSW $X1 $Y1 $X2 $Y2) 
   (let $next_box 
        ; make sure valid square lookup possible
        (if (and (and (< $X1 9) (> $X1 0)) (and (< $Y1 9) (> $Y1 0))) 
          ;return all the contents of the square
          (return_entire_box ($X1 $Y1)) 
          ; else in this case, either x or y is invalid, setting to size-atom 0 will result in False below.
          ()  
          )
        (if (and (== $Y1 $Y2) (== $X1 $X2))
          True
              ; Not on destination, check for empty passing square
          (if (== (size-atom $next_box) 2)
              (clearcheckSW (- $X1 1) (- $Y1 1) $X2 $Y2)
              ; if there is a piece in box we cannot move through this square (size 4 is occupied)
              False))))

;******************************
; E N D    clear_route routines
;******************************

;***************************************************************
; Function:     replace_square_recursive
; Description:  Recursively scans and rewrites the board list,
;               removing a piece from its original square and
;               placing it on a new square with updated attributes.
;
; Input:        $X1, $Y1   - Coordinates of the piece's origin
;               $color     - Color of the piece being moved
;               $rank      - Rank/type of the piece
;               $X2, $Y2   - Coordinates of the destination square
;               $current_board - List of all squares on the board
;
; Output:       A new board list with the piece moved from ($X1 $Y1)
;               to ($X2 $Y2), and all other squares preserved.
;***************************************************************
(: (replace_square_recursive) (-> Atom Atom Atom Atom Atom Atom Expression Expression))
(= (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 $current_board) 
   (if (== (size-atom $current_board) 0)
        ()
        (let* (
          ($next-sq (car-atom $current_board))
          ($X (nth 1 $next-sq))
          ($Y (nth 2 $next-sq))
              )
        (if (and (== $X $X1) (== $Y $Y1))
          ; if we are on the square being moved FROM, then remove piece color and rank since it will disappear
          (let $from_square ($X1 $Y1)   ; <-- note no color and rank
               (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $from_square $rest)))
        (if (and (== $X $X2) (== $Y $Y2))
          ; elif on square being moved INTO, use the moving piece's color and rank.
          (let $into_square ($X2 $Y2 $color $rank)  ; <-- color and rank in destination  
               (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $into_square $rest)))
          ; otherwise just keep chugging through the board...
        (let $rest (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 (cdr-atom $current_board)) 
                          (cons-atom $next-sq $rest)))))))  ; <-- here we just copy the existing square

;***************************************************************
; Function:     move_piece_on_board
; Description:  Parses and moves a piece from one square to another
;               using `replace_square_recursive`. Also prints
;               move and capture messages to the console.
;
; Input:        $starting_square - List (X1 Y1 color rank)  
;               $target_square   - List (X2 Y2) or (X2 Y2 opponent_color opponent_rank)  
;               $current_board   - Current list representing the game board
;
; Output:       A new board list with updated piece locations.
;***************************************************************
(= (move_piece_on_board $starting_square $target_square $current_board) 
  (let* (
    ($X1  (nth 1 $starting_square))
    ($Y1  (nth 2 $starting_square))
    ($color (nth 3 $starting_square))
    ($rank  (nth 4 $starting_square))
    ($X2  (nth 1 $target_square))
    ($Y2  (nth 2 $target_square))
    ; move piece on chessboard list
    ($new_board (replace_square_recursive $X1 $Y1 $color $rank $X2 $Y2 $current_board))
    ; messages for user
    ($starting_square_lettered (convert_x_letter $starting_square))
    ($target_square_lettered (convert_x_letter $target_square))

    ($print0 (println! ( ("Moving from: "
                          $starting_square_lettered " to: " $target_square_lettered))))
    ;($print0 (println! (format-args "Moving from: {} to: {}" 
    ;    ($starting_square_lettered $target_square_lettered))))
    ($print1 (if (== (size-atom $target_square) 2)
             ()
             (println! "Piece captured!")))
    ($print2 (if (== (size-atom $target_square) 2)
             ()
             (println! $target_square_lettered)))
    ) 
    ; return new chessboard list
    $new_board))

;***************************************************************
; Function:     reset_square
; Description:  Removes any existing atom representing a square 
;               at the given coordinates, whether it's occupied 
;               or empty.
;
; Input:        $X - X coordinate of the square
;               $Y - Y coordinate of the square
;
; Output:       Atomspace is updated with the square at ($X $Y) 
;               removed, regardless of its content.
;***************************************************************
(= (reset_square $X $Y)
    (remove-atom &self (square $X $Y $anycolor $anyrank)))  ; piece captured!
(= (reset_square $X $Y)
    (remove-atom &self (square $X $Y)))  ; empty square
    
;***************************************************************
; Function:     move_piece
; Description:  Updates the Atomspace to move a piece from a 
;               source square to a destination square. This 
;               includes removing the original atom, updating 
;               square occupancy, and handling any captures.
;
; Input:        ($X1 $Y1 $color $rank) - Starting square (full piece info)
;               ($X2 $Y2)              - Target square (coordinates only)
;
; Output:       True - The move has been executed in the Atomspace
;***************************************************************
(= (move_piece ($X1 $Y1 $color $rank) ($X2 $Y2)) 
  (let* (
    ($starting_square_complete_atom (square $X1 $Y1 $color $rank))
    ($_1 (remove-atom &self $starting_square_complete_atom))
    ; create atom for empty starting square
    ($old_square_complete (square $X1 $Y1))
    ($_2 (add-atom &self $old_square_complete))
    ; delete atom for whatever occupies the new square, if anything
    ($_3 (collapse (reset_square $X2 $Y2)))  ; "collapse" needed since will return "()" if fail, not Empty
    ; add piece at new square
    ($new_square_complete (square $X2 $Y2 $color $rank))
    ($_4 (add-atom &self $new_square_complete))      
        ) 
    True))

;***************************************************************
; Function:     reset_pieces
; Description:  Reverts a previously executed move in the Atomspace.
;               It restores either an empty target or a captured 
;               opponent's piece, depending on input shape.
;
; Input:        Case 1 (empty target): 
;                   ($X1 $Y1 $color $rank) - Moved piece's original square
;                   ($X2 $Y2)              - Target square (empty)
;
;               Case 2 (captured piece): 
;                   ($X1 $Y1 $color1 $rank1) - Moved piece original square
;                   ($X2 $Y2 $color2 $rank2) - Target square with captured piece
;
; Output:       True - Atomspace state has been restored to prior configuration
;***************************************************************
(= (reset_pieces ($X1 $Y1 $color $rank) ($X2 $Y2))
  (let $_1
   (move_piece ($X2 $Y2 $color $rank) ($X1 $Y1)) True))         ;restore original complete start
; if piece was to capture an opponent's piece
(= (reset_pieces ($X1 $Y1 $color1 $rank1) ($X2 $Y2 $color2 $rank2))
  (let*
    (
   ($_1 (move_piece ($X2 $Y2 $color1 $rank1) ($X1 $Y1)))       ; restore original starting piece
   ($_2 (remove-atom  &self  (square $X2 $Y2)))                  
   ($_3 (add-atom &self (square $X2 $Y2 $color2 $rank2)))    ; restore original destination piece
    )
    True)
)

;***************************************************************
; Function:     get_score
; Description:  Returns the material value of each piece of a given color.
;               Used for computing hypothetical board evaluations.
;
; Input:        $color - Either `s` (silver/human) or `g` (gold/AI)
;
; Output:       A list of values corresponding to the ranks found.
;               Piece values: p=1, n=3, b=3, r=5, q=9
;***************************************************************
(= (get_score $color)
    (match &self (square $X $Y $color $rank)
            (case $rank
                    (
                    (p 1)
                    (n 3)
                    (b 3)
                    (r 5)
                    (q 9)
                    ))))

;***************************************************************
; Function:     human_player_attacking
; Description:  Identifies AI-controlled pieces currently under
;               threat from the human player and returns their value.
;
; Input:        None (matches from Atomspace)
;
; Output:       A list of vulnerable AI piece values (e.g., (3 1))
;***************************************************************
(= (human_player_attacking)
       (match &self (square $x $y g $AI_rank) 
          (let $BoolVulnerable (take_dest ($x $y) s)
              (if (== $BoolVulnerable True)
                  (case $AI_rank
                    (
                    (p 1)
                    (n 3)
                    (b 3)
                    (r 5)
                    (q 9)
                    ))
                  (empty)))))

;***************************************************************
; Function:     score_move
; Description:  Evaluates a hypothetical move by scoring the outcome.
;               Scores both the total opponent material and the AI
;               vulnerability after the move.
;
; Input:        ($X1 $Y1 $color $rank) - AI piece being moved
;               ($X2 $Y2)              - Destination square
;
; Output:       Adds an atom of the form:
;               (score X1 Y1 color rank X2 Y2 OpponentScore AIScore)
;***************************************************************
(= (score_move ($X1 $Y1 $color $rank) ($X2 $Y2))
  (let*
      (
      ($all_opponent_piece_values (collapse (get_score s)))
      ($Opponent_hypothetical_score (addit_list $all_opponent_piece_values))
      ($all_vulnerable_AI_piece_values (collapse (human_player_attacking)))
      ($AI_hypothetical_score (addit_list $all_vulnerable_AI_piece_values))
      ; compute AI worst case score
      )  
      (add-atom &self 
        (score $X1 $Y1 $color $rank $X2 $Y2 $Opponent_hypothetical_score $AI_hypothetical_score))))
    
;***************************************************************
; Function:     checkking
; Description:  Determines whether the specified king is currently
;               in check from the opponent's pieces.
;
; Input:        $king_side_to_check - Either `s` (silver/human) or
;                                     `g` (gold/AI), representing the
;                                     side whose king is being checked.
;
; Output:       True  - if the king is in check
;               False - if the king is not in check
;***************************************************************
(= (checkking $king_side_to_check)  
  (let*
    (
    ; locate king
    ($king_square (xy_box ($king_side_to_check k)))    
    ; 
    ($full_king_sq (return_entire_box $king_square))  
    )
    ; Ensure the king is not in check, return True or False (True=Check)
    (if (== $king_side_to_check s) 
        (take_dest $full_king_sq g)
        (take_dest $full_king_sq s)
    )))

;***************************************************************
; Function:     any_moves_to_escape
; Description:  Checks whether the specified side has any legal moves
;               to escape check (or avoid checkmate). Evaluates both
;               moves to empty squares and captures of opponent pieces.
;
; Input:        $side - The color of the side being checked for legal
;                       moves (e.g., `s` for silver/human or `g` for gold/AI).
;
; Output:       A list containing one successful move that would remove the
;               king from check, if any exist.
;               Returns Empty if no such moves are found.
;***************************************************************
; variation to move any piece to an empty square.
(= (any_moves_to_escape $side) 
    ; match all squares occupied by $side (eg., s or g)
    (match &self (square $x1 $y1 $side $rank)
       ; match all empty squares
       (match &self (square $x2 $y2) 
          ; check all possible moves to empty squares
          (if (== (clear_route ($x1 $y1 $side $rank) ($x2 $y2)) True)
            ; if there is a route to empty square, move pieces in atomspace and see if king in check
            (let*
              (
                ; move piece in atomspace
                ($__1 (move_piece ($x1 $y1 $side $rank) ($x2 $y2)))
                ($king_status (checkking $side))
                ; move piece back in atomspace (we are just envisioning possibilities now)
                ($__2 (reset_pieces ($x1 $y1 $side $rank) ($x2 $y2)))

              )
              (if (== $king_status False)
                    ; we have found that a piece can block check
                    (($x1 $y1 $side $rank) ($x2 $y2))
                    (empty)))
            (empty)))))
; variation to capture any piece whatsoever
(= (any_moves_to_escape $side) 
  ; assign a variable with the opposing side color
  (let $otherside (if (== $side g) s g) 
    ; match all squares occupied by $side (eg., s or g)
    (match &self (square $x1 $y1 $side $rank)
       ; match all occupied squares by other side
       (match &self (square $x2 $y2 $otherside $rank_capture_piece) 
          ; check all possible moves to occupied squares
          (if (== (clear_route ($x1 $y1 $side $rank) ($x2 $y2)) True)
            ; if there is a route to occupied square, move pieces in atomspace and see if king in check
            (let*
              (
                ; move piece in atomspace
                ($__1 (move_piece ($x1 $y1 $side $rank) ($x2 $y2)))
                ($king_status (checkking $side))
                ; move piece back in atomspace (we are just envisioning possibilities now)
                ($__2 (reset_pieces ($x1 $y1 $side $rank) ($x2 $y2 $otherside $rank_capture_piece)))
              )
              (if (== $king_status False)
                    ; we have found a move that can block check
                    (($x1 $y1 $side $rank) ($x2 $y2))
                    (empty)))
            (empty))))))

;***************************************************************
; Function:     takingboxes
; Description:  Retrieves all squares occupied by pieces of a given color.
;               Used to determine potential threats posed by the opponent.
;
; Input:        $OpponentColor - The color of the opponent's pieces (e.g., `s` or `g`)
;
; Output:       A list of squares, each in the form ($x $y $OpponentColor $rank),
;               representing all positions occupied by the specified color.
;***************************************************************
(= (takingboxes $OpponentColor)
  (match &self (square $x $y $OpponentColor $p) 
      ($x $y $OpponentColor $p)))

; Obsolete, for comparison only
; (takingboxes_recursive)
;
; Input:  pieces color to look for, board
; Output: a list of all pieces belonging to a given color w/ square coordinates.
(= (takingboxes_recursive $OpponentColor $Board)
   (if (== (size-atom $Board) 0)
        ; end of the line, return empty list
        ()
        ; examine next square for opponent piece
        (let $next-sq (car-atom $Board)
             ; if size = 2, square is empty
             (if (== (size-atom $next-sq) 2) 
                  (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) $rest)
                  ;else check color
                  (let $NextColor (nth 3 $next-sq)
                      (if (== $NextColor $OpponentColor)
                        ; if same, add this square to the list to return
                        (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) (cons-atom $next-sq $rest))
                        (let $rest (takingboxes_recursive $OpponentColor (cdr-atom $Board)) $rest)))))))

;***************************************************************
; Function:     list_clear_route / list_clear_route_recursion
; Description:  Determines which pieces from a list can reach (and potentially capture) 
;               a given square using legal movement rules.
;
; Input:        $Square     - The target square potentially under threat (e.g., ($x $y))
;               $CanAttack  - A list of attacker piece positions (e.g., (($x1 $y1 g q) ...))
;
; Output:       A list of positions from $CanAttack that have a clear route to $Square.
;               If no attackers have a clear route, the result is an empty list.
;
; Notes:
;   - `list_clear_route` evaluates a single attacker non-deterministically (via `superpose`).
;   - `list_clear_route_recursion` deterministically walks the entire list and builds up
;     a filtered list of all successful attackers.
;***************************************************************
(= (list_clear_route $Square $CanAttack)
    (let*
      (
      ($next-sq (superpose $CanAttack))
      ($AttackBool (clear_route $next-sq $Square))
      )
      (if (== $AttackBool True) $next-sq (empty))))

; Obsolete for comparison
; (list_clear_route)
;
; Input:  $Square that might be compromised, all pieces available to attack square in question, $CanAttack
; Output: list of pieces that can take the piece in the square in question
(= (list_clear_route_recursion $Square $CanAttack)
   (if (== (size-atom $CanAttack) 0)
       ; end of the line
       ()
       ; check if next piece in $CanAttack can capture $Square, if so add to return list.
       (let* (
          ($next-sq (car-atom $CanAttack))
          ($AttackBool (clear_route $next-sq $Square))
             )
          (if (== $AttackBool True) 
              (let $rest (list_clear_route $Square (cdr-atom $CanAttack)) (cons-atom $next-sq $rest))
              (let $rest (list_clear_route $Square (cdr-atom $CanAttack)) $rest)
          )
       )
   ) 
)



;*******************************************************
; Basic commands.  The game is executed using the following commands.
;*******************************************************

;***************************************************************
; Description:  Core command handlers for Greedy Chess gameplay.
;               Enables human/AI move input, game state transitions, 
;               board initialization, command help, and board display.
;
; Commands:
;   - (chess)   :: Initializes or restarts the board and starts the game.
;   - (M)       :: Handles human player's move input and execution.
;   - (G)       :: Handles AI (Greedy) move logic and execution.
;   - (R)       :: Resets and reinitializes the game.
;   - (C)       :: Displays the available command list.
;   - (D)       :: Prints the current state of the board.
;
; Input:        None (invoked interactively or from REPL).
; Output:       Side effects on Atomspace, updates and console output.
;
; Notes:
;   - Internally maintains game-state using (game-state $msg).
;   - Board is represented by the atom (board-state $board).
;   - Supports in-game status updates like check, checkmate, and move feedback.
;***************************************************************

; This command must be invoked at startup with "!(chess)"
(= (chess) 
  (match &self (game-state $msg)
    ; if first invocation, just create board
    (if (==  initializing   $msg) ; then  
      (progn 
        (change-game-state started)
        ; create the board for the first time
        (add-atom &self 
          (board-state ((1 8 g r) (2 8 g n) (3 8 g b) (4 8 g q) (5 8 g k) (6 8 g b) (7 8 g n) (8 8 g r) 
                        (1 7 g p) (2 7 g p) (3 7 g p) (4 7 g p) (5 7 g p) (6 7 g p) (7 7 g p) (8 7 g p) 
                        (1 6) (2 6) (3 6) (4 6) (5 6) (6 6) (7 6) (8 6) 
                        (1 5) (2 5) (3 5) (4 5) (5 5) (6 5) (7 5) (8 5) 
                        (1 4) (2 4) (3 4) (4 4) (5 4) (6 4) (7 4) (8 4) 
                        (1 3) (2 3) (3 3) (4 3) (5 3) (6 3) (7 3) (8 3) 
                        (1 2 s p) (2 2 s p) (3 2 s p) (4 2 s p) (5 2 s p) (6 2 s p) (7 2 s p) (8 2 s p) 
                        (1 1 s r) (2 1 s n) (3 1 s b) (4 1 s q) (5 1 s k) (6 1 s b) (7 1 s n) (8 1 s r)))
        )
        ; display welcome messages and board
        (welcome))
    ; elif there has already been one game played
    (if (==  restarted   $msg) ; then
      (progn 
        ; remove the old chess board
        (match &self (board-state $old_board) (remove-atom &self (board-state $old_board)))
        ; re-create a new board 
        (add-atom &self           
          (board-state ((1 8 g r) (2 8 g n) (3 8 g b) (4 8 g q) (5 8 g k) (6 8 g b) (7 8 g n) (8 8 g r) 
                        (1 7 g p) (2 7 g p) (3 7 g p) (4 7 g p) (5 7 g p) (6 7 g p) (7 7 g p) (8 7 g p) 
                        (1 6) (2 6) (3 6) (4 6) (5 6) (6 6) (7 6) (8 6) 
                        (1 5) (2 5 s r) (3 5 g q) (4 5) (5 5) (6 5) (7 5) (8 5) 
                        (1 4) (2 4) (3 4) (4 4) (5 4) (6 4) (7 4) (8 4) 
                        (1 3) (2 3) (3 3) (4 3) (5 3) (6 3) (7 3) (8 3) 
                        (1 2 s p) (2 2 s p) (3 2 s p) (4 2 s p) (5 2 s p) (6 2 s p) (7 2 s p) (8 2 s p) 
                        (1 1 s r) (2 1 s n) (3 1 s b) (4 1 s q) (5 1 s k) (6 1 s b) (7 1 s n) (8 1 s r)))
        )
        ;
        ; display welcome messages and board
        (welcome))
    (; else if
        empty))))) 

;(: M (-> command))
(= (M)  
  (if (== (game-still-playing) False)
    (println! "Game over. Please reset to play again (enter r).")
    ; else keep playing
    (progn 
    ; prompt for x/y location
    ; temporary change for PeTTa test, enter eg ((1 2) (1 3))
    ;(let $coord (prompt-for-move) ())
    (let $coord (readln!) ())
    (let $f (nth 1 $coord) ())
    (let $g (nth 2 $coord) ())
    ; get the current board
    (match &self (board-state $starting_board) $starting_board)
    ; reset, then add each piece to atomspace to work with individually with form eq '(square 1 1 s r)'
    (reset-pieces $starting_board)
    (let $entire_source (return_entire_box $f) ())
    ; check if source location and destination are not the same, and if there is piece to move.
    (if (and (not (== $f $g)) (== (size-atom $entire_source) 4))                            
     ; check if destination is empty or destination has opponent's piece  
      (progn 
      ; Retrieve the source box details.
      (let $h (return_entire_box $f) ())
      ; Get source box color, assign NIL if no piece
      (let $source_color (if (== (size-atom $h) 4) (nth 3 $h) NIL) ())
      ; Retrieve the destination box details.
      (let $i (return_entire_box $g) ()) 
      (let $dest_color (if (== (size-atom $i) 4) 
                           (nth 3 $i)
                            NIL) ())
      ; Check there is a human piece to move.
      (if (and (== $source_color s)                                    
               (or  
               ; OK to move to open square size 2...
               (== (size-atom $i) 2)     
               ; OK to take another piece, check color is not the same...
               (and (== (size-atom $i) 4) (not (== (nth 3 $h) $dest_color)))))
        ; Validate the path for the piece.
        (if (== (clear_route $h $i) True)               
          ; Try moving piece provisionally
          (let* ( 
              ; Try the move and envision the board state.   
              ($MoveBool  (move_piece $h $g))   
              ; Ensure the human player's king (s) is not in check from gold (g) if moved.   
              ($KingCompromised (checkking s))
                  ) 
              ; If move valid, proceed to create new board atom with move.
              (if (== $KingCompromised True)
                  (println! "Can't move there, your king would be in check.")
                  ;else
                  (progn 
                  ; re-create a new board 
                  (println! "Moving your piece...")
                  (let $provisional_board (move_piece_on_board $h $i $starting_board)
                       (add-atom &self (board-state $provisional_board)))
                  ; remove the old chess board
                  (remove-atom &self (board-state $starting_board))
                  ; display
                  (D) 
                  ; announce CHECK or CHECKMATE of gold player (AI)
                  (let $Check (checkking g)
                      (if (== $Check True)
                          ; if the AI is in check, see if AI has any moves whatsoever left to escape
                          (let $any_moves_left_whatsoever (collapse (any_moves_to_escape g))
                            (if (== $any_moves_left_whatsoever ()) ;an empty list means no moves!
                                ; if no moves possible for AI
                                (progn
                                  (println! "Checkmate! You win!")
                                  (println! "Winner!  Winner!  Winner!")
                                  (println! "")
                                  (change-game-state checkmate)
                                )
                                (println! "Check! Enter g for Greedy Chess move.")))
                          (println! "Enter g for Greedy Chess move.")  ))
                  )
              )
          )
          (println! "Can't move piece there."))
        ; else (moving to location with same piece or other problem)
        (println! "Invalid move.")
      )
      )
      ;else (trying to move to same location)
      (println! "Invalid move.")) 
    ; since move complete, delete any individual square atoms which only live for a single move.
    (delete-pieces)
    )))

; AI move piece command
;(: G (-> command))
(= (G)  
  (match &self (game-state $msg)
  ; check for game over...
    (if (or (==  checkmate   $msg) (==  resigned $msg)) 
      ; then  
      (println! "Game over. Please reset to play again (enter r).")
      ;else keep playing...
      (progn 
        ; get the current board
        (match &self (board-state $starting_board) $starting_board)
        ; Add each piece to &self atomspace to work with individually with form eq '(square 1 1 s r)'.
        ; These atoms are used to "envision" moves only. So if you alter these atoms, when done immediately
        ; reverse back to the starting state. 
        ; You can use move_piece and reset_pieces to respectively envision a move, then reverse the move.
        (reset-pieces $starting_board)
        ; determine next AI move
        (let $move (decide_greedy_move) NIL )  
        (if  (== $move ())
          (let*
            ; if all attempts fail, locate AI king and see if he is in checkmate
            (
            ($king_square (xy_box (g k)))    
            ($full_king_sq (return_entire_box $king_square))  
            ($KingCompromised (take_dest $full_king_sq s))   
            )
            (if (== $KingCompromised True)
              (progn
                (println! "Checkmate! You win!")
                (println! "Winner!  Winner!  Winner!")
                (println! "")
                (change-game-state checkmate)
                )
              (progn
                (println! "Greedy Chess cannot find a good move. Game over. You win!")
                (change-game-state resigned))))
          ; else proceed with move
          (progn
            (let $start (nth 1 $move) ())
            (let $destination (nth 2 $move) ()) ; this is (x y) only
            ; we need the entire present destination box to pass to the board display
            (let $destination_entire (return_entire_box $destination) ())
            ; make the move in atomspace using the individual "square" atoms
            (move_piece $start $destination) ; the envision move persists in atomspace "square" atoms until end of routine.
            (println! "Greedy Chess moving...")
            ; create atom for the updated complete chess board...
            (let $provisional_board (move_piece_on_board $start $destination_entire $starting_board)
                  (add-atom &self (board-state $provisional_board)))
            ; remove the old chess board
            (remove-atom &self (board-state $starting_board))
            ; display the new board 
            (D)
            ; announce CHECK or CHECKMATE of human player if applicable
            ; Note: The attemptcheckmate routine will set the checkmate status.
            (match &self (game-state $msg2) 
              (if (==  checkmate   $msg2) 
                (progn
                            (println! "Checkmate! Greedy Chess wins!!!")
                            (println! "Good game!")
                            (println! "")
                          )
                ; else look for check...
                (let $Check (checkking s)
                  (if (== $Check True)        
                    (println! "Check!")
                    (println! "Your move.")))))
            ; since move complete, delete any individual square atoms which only live for a single move.     
            ;     "
          )
        )
        (delete-pieces)
      )
    )
  )
)

; The reset command will start the game over.
;(: R (-> command))
(= (R) 
  (progn 
    (change-game-state restarted)
    (chess)))

; The "commands" command just lists the available commands.
;(: C (-> command))
(= (C) 
  (progn 
    (println! " ") (println! " ") (println! " ") (println! " ")
    (println! "- Your pieces are white at bottom.")
    (println! "- Please take note of the following simple commands:")
    (println! "*------- C o m m a n d s ------------*")
    (println! "MOVE YOUR PIECE USE example      ->  m <ENTER> a2a3 <ENTER>")
    (println! "   Result:  YOUR pawn in a2 moved to location a3.")
    (println! "Greedy Chess Move (AI)           ->  g")
    (println! "Reset & Replay                   ->  r")
    (println! "Commands List                    ->  c")
    (println! "Display Board                    ->  d")
    (println! "Quit                             ->  q")))
;

; The display command shows the present board.
;(: D (-> command))
;(= (D) (display_board (match &self (board-state $board) $board)))
(= (D) (match &self (board-state $board) (println! $board)))


;*******************************************************
;   M A I N   C O M M A N D   L O O P
;*******************************************************

;***************************************************************
; Function:     main_loop / command-loop
; Description:  Main entry point and user interaction loop for
;               MeTTa Greedy Chess. Provides REPL-style input
;               handling for player and AI moves, game control,
;               and board interaction.
;
; Input:        None (invoked from REPL or startup script)
;
; Output:       Executes commands based on user input and updates
;               Atomspace and board display accordingly.
;
; Notes:
;   - Recursively loops until the user enters "q"
;   - Uses (case ...) to map string input to handler atoms
;***************************************************************

(= (command-loop)
  (progn 
    (println! "Please enter your command.")
    (let $command (readln!)   ; get user command
        (case $command
          (
          (m  (M))                         ; Move human piece 
          (g  (G))                         ; AI move (Greedy)
          (r  (R))                         ; Reset = "r"
          (c  (C))                         ; List valid commands = "c"
          (d  (D))                         ; Display board "d"
          (q  (println! "Quitting MeTTa Greedy Chess."))
          ($_ (println! "Invalid command, please try again or enter c for a list of commands."))
          )))
    (if (== $command q) 
      ()
      (command-loop))
      ))

;-----------;
(= (main_loop) 
  (progn
    (chess)           ; Start the chess game
    (command-loop)))  ; Enter the recursive command-processing loop until quit command 'q'
!(main_loop)
