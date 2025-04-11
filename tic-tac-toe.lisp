(defparameter *current-board* (make-array '(3 3) :initial-element "-"))
(defparameter *current-player* "X")
(defparameter *player-1-ai* NIL)
(defparameter *player-2-ai* NIL)
(defconstant NUM_ROWS 3)
(defconstant NUM_COLS 3)
(defconstant NEGATIVE_INFINITY -99999999)
(defconstant WIN_SCORE 10)
(defconstant TIE_SCORE 0)
(defconstant INVALID_MOVE_SCORE -9999999)
(defconstant LOSS_SCORE -10)

(defun play-game ()
  (format t "~%Hey there! Let's play some Tic Tac Toe!~%")
  (loop while (not (game-over))
        do (play-turn))
  (announce-winner))

(defun game-over ()
  (or
   (tie-board *current-board*)
   (winning-board *current-board* "X")
   (winning-board *current-board* "O")))

(defun tie-board (board)
  (not
   (or
    (string= (get-cell board 0 0) "-")
    (string= (get-cell board 0 1) "-")
    (string= (get-cell board 0 2) "-")
    (string= (get-cell board 1 0) "-")
    (string= (get-cell board 1 1) "-")
    (string= (get-cell board 1 2) "-")
    (string= (get-cell board 2 0) "-")
    (string= (get-cell board 2 1) "-")
    (string= (get-cell board 2 2) "-"))))

(defun winning-board (board player)
  (or (three-in-a-row board player 0 0 0 1 0 2)
      (three-in-a-row board player 1 0 1 1 1 2)
      (three-in-a-row board player 2 0 2 1 2 2)
      (three-in-a-row board player 0 0 1 0 2 0)
      (three-in-a-row board player 0 1 1 1 2 1)
      (three-in-a-row board player 0 2 1 2 2 2)
      (three-in-a-row board player 0 0 1 1 2 2)
      (three-in-a-row board player 0 2 1 1 2 0)))

(defun three-in-a-row (board player row1 col1 row2 col2 row3 col3)
  (and (string= (get-cell board row1 col1) player)
       (string= (get-cell board row2 col2) player)
       (string= (get-cell board row3 col3) player)))

(defun set-cell (board row col cell)
  (setf (aref board row col) cell))

(defun get-cell (board row col)
  (aref board row col))

(defun play-turn ()
  (print-game-board)
  (format t "Player ~A, it's your turn.~%~%" (player-number *current-player*))
  (apply #'make-move (get-row-col))
  (setq *current-player* (other-player *current-player*)))

(defun print-game-board ()
  (format t "~%~%~%===GAME BOARD===~%")
  (loop for i from 0 below NUM_ROWS do
          (loop for j from 0 below NUM_COLS do
                  (format t " ~A " (aref *current-board* i j)))
          (format t "~%"))
  (format t "~%~%"))

(defun player-number (current-player)
  (if (string= current-player "X") 1 2))

(defun make-move (row col)
  (set-cell *current-board* row col *current-player*))

(defun get-row-col ()
  (if (ai-player)
      (get-row-col-ai)
      (get-row-col-user-input)))

(defun ai-player ()
  (if (eql (player-number *current-player*) 1)
      *player-1-ai*
      *player-2-ai*))

(defun get-row-col-ai ()
  (let
      ((max-score NEGATIVE_INFINITY)
       (best-move-row NIL)
       (best-move-col NIL))
    (loop for i from 0 below NUM_ROWS do
            (loop for j from 0 below NUM_COLS do
                    (when (available-move *current-board* i j)
                          (let ((score (score-move *current-board* *current-player* i j)))
                            (when (> score max-score)
                                  (setf max-score score
                                    best-move-row i
                                    best-move-col j))))))
    (list best-move-row best-move-col)))

(defun score-move (board player row col)
  (let ((board-copy (copy-board-make-move board player row col))
        (the-other-player (other-player player)))
    (cond
     ((winning-board board-copy player) WIN_SCORE)
     ((winning-board board-copy the-other-player) LOSS_SCORE)
     ((tie-board board-copy) TIE_SCORE)
     ((not (available-move board row col)) INVALID_MOVE_SCORE)
     (t (- (apply #'max
               (loop for i from 0 below NUM_ROWS append
                       (loop for j from 0 below NUM_COLS
                               when (available-move board-copy i j)
                             collect (score-move board-copy the-other-player i j)))))))))

(defun get-row-col-user-input ()
  (list (get-row) (get-col)))

(defun get-row ()
  (format t "~%Enter a row: ")
  (read-int))

(defun get-col ()
  (format t "~%Enter a col: ")
  (read-int))

(defun read-int ()
  (parse-integer (read-line)))

(defun other-player (current-player)
  (if (string= current-player "X") "O" "X"))

(defun announce-winner ()
  (cond
   ((tie-board *current-board*) (format t "~%~%It's a tie!"))
   ((winning-board *current-board* "X") (format t "~%~%Player 1 wins!"))
   ((winning-board *current-board* "O") (format t "~%~%Player 2 wins!"))))

(defun available-move (board row col)
  (string= (get-cell board row col) "-"))

(defun copy-board-make-move (board player row col)
  (let ((copy (make-array '(3 3) :initial-element "-")))
    (set-cell copy 0 0 (get-cell board 0 0))
    (set-cell copy 1 0 (get-cell board 1 0))
    (set-cell copy 2 0 (get-cell board 2 0))
    (set-cell copy 0 1 (get-cell board 0 1))
    (set-cell copy 1 1 (get-cell board 1 1))
    (set-cell copy 2 1 (get-cell board 2 1))
    (set-cell copy 0 2 (get-cell board 0 2))
    (set-cell copy 1 2 (get-cell board 1 2))
    (set-cell copy 2 2 (get-cell board 2 2))
    (set-cell copy row col player)
    copy))

(defun play-game-user-user ()
  (setf *player-1-ai* NIL)
  (setf *player-2-ai* NIL)
  (play-game))

(defun play-game-user-ai ()
  (setf *player-1-ai* NIL)
  (setf *player-2-ai* T)
  (play-game))

(defun play-game-ai-ai ()
  (setf *player-1-ai* T)
  (setf *player-2-ai* T)
  (play-game))

;; tests
(defun run () (load "tic-tac-toe.lisp"))

(defun test-score-move-basic ()
  (score-move *current-board* "X" 0 0))

(defun test-score-move-1 ()
  (setf *current-board*
    (make-array '(3 3)
      :initial-contents '(("X" "O" "-")
                          ("-" "X" "-")
                          ("-" "-" "-"))))
  (setf *current-player* "O")
  ; (get-row-col-ai)
  (format t "score-move(0, 2) ~A~%" (score-move *current-board* "O" 0 2))
  (format t "score-move(2, 0) ~A~%" (score-move *current-board* "O" 2 0)))

(defun test-score-move-2 ()
  (setf *current-board*
    (make-array '(3 3)
      :initial-contents '(("X" "O" "X")
                          ("O" "X" "X")
                          ("O" "-" "-"))))

  ; score-move("X", 2, 2) => WIN - 10
  (format t "score-move('X', 2, 2) => ~A~%" (score-move *current-board* "X" 2 2))
  ; score-move("O", 2, 2) => TIE - 0
  (format t "score-move('O', 2, 2) => ~A~%" (score-move *current-board* "O" 2 2))
  ; score-move("X", 2, 1) => LOSS - -10
  (format t "score-move('X', 2, 1) => ~A~%" (score-move *current-board* "X" 2 1)))

(defun test-score-move-3 ()
  (setf *current-board*
    (make-array '(3 3)
      :initial-contents '(("O" "-" "X")
                          ("X" "-" "-")
                          ("X" "O" "O"))))

  ; get-row-col-ai => 1, 1
  (setf *current-player* "X")
  (format t "AI suggested next move is 1,1 => ~A~%" (same-elements (get-row-col-ai) (list 1 1)))

  ; score-move("X", 0, 1) => -10
  (format t "score-move(0, 1) is -10 => ~A~%" (eql (score-move *current-board* "X" 0 1) -10))
  ; score-move("X", 1, 2) => -10
  (format t "score-move(1, 2) is -10 => ~A~%" (eql (score-move *current-board* "X" 1 2) -10))
  ; score-move("X", 1, 1) => 10
  (format t "score-move(1, 1) is 10 => ~A~%" (eql (score-move *current-board* "X" 1 1) 10)))

(defun test-score-move-4 ()
  (setf *current-board*
    (make-array '(3 3)
      :initial-contents '(("-" "X" "-")
                          ("-" "-" "X")
                          ("O" "O" "X"))))

  ; get-row-col-ai => 1, 1
  (setf *current-player* "X")
  (format t "AI suggested next move is 1,1 => ~A~%" (same-elements (get-row-col-ai) (list 1 1))))

(defun same-elements (l1 l2)
  (and (subsetp l1 l2) (subsetp l2 l1)))

(defun test-get-row-col-ai ()
  (setf *current-board* (make-array '(3 3) :initial-element "-"))
  (setf *current-player* "X")
  (get-row-col-ai))

(defun test-user-vs-ai ()
  (setf *current-board* (make-array '(3 3) :initial-element "-"))
  (setf *current-player* "X")
  (setf *player-1-ai* NIL)
  (setf *player-2-ai* T)
  (play-game))
