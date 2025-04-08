(defparameter *game-board* (make-array '(3 3) :initial-element "-"))
(defparameter *current-player* "X")

(defun play-game ()
  (format t "~%Hey there! Let's play some Tic Tac Toe!~%~%")
  (loop while (not (game-over))
        do (play-turn))
  (announce-winner))

(defun game-over () (or
                     (horizontal-3-in-a-row)
                     (vertical-3-in-a-row)
                     (diagonal-3-in-a-row)))

(defun play-turn ()
  (print-game-board)
  (format t "Player ~A, it's your turn.~%~%" (player-number *current-player*))
  (make-move (get-row) (get-col))
  (setq *current-player* (other-player *current-player*)))

(defun print-game-board ()
  (format t "~%Game board:~%")
  (loop for i from 0 below (array-dimension *game-board* 0) do
          (loop for j from 0 below (array-dimension *game-board* 1) do
                  (format t " ~A " (aref *game-board* i j)))
          (format t "~%"))
  (format t "~%~%"))

(defun get-row ()
  (format t "Enter a row: ")
  (read-int))

(defun get-col ()
  (format t "Enter a col: ")
  (read-int))

(defun read-int ()
  (parse-integer (read-line)))

(defun make-move (row col)
  (set-cell row col *current-player*))

(defun other-player (current-player)
  (if (string= current-player "X") "O" "X"))

(defun player-number (current-player)
  (if (string= current-player "X") 1 2))

(defun announce-winner ()
  (print "TODO: announce-winner"))

(defun evaluate-winner (current-player)
  (or (compare-three 0 0 0 1 0 2 current-player)
      (compare-three 1 0 1 1 1 2 current-player)
      (compare-three 2 0 2 1 2 2 current-player)
      (compare-three 0 0 1 0 2 0 current-player)
      (compare-three 0 1 1 1 2 1 current-player)
      (compare-three 0 2 1 2 2 2 current-player)
      (compare-three 0 0 1 1 2 2 current-player)
      (compare-three 0 2 1 1 2 0 current-player)))

(defun compare-three (row1 col1 row2 col2 row3 col3 player)
  (and (string= (get-cell row1 col1) player)
       (string= (get-cell row2 col2) player)
       (string= (get-cell row3 col3) player)))

; (defun horizontal-3-in-a-row ()
;   (or
;    (check-3-in-a-row 0 0 0 1)
;    (check-3-in-a-row 1 0 0 1)
;    (check-3-in-a-row 2 0 0 1)))

; (defun vertical-3-in-a-row ()
;   (or
;    (check-3-in-a-row 0 0 1 0)
;    (check-3-in-a-row 0 1 1 0)
;    (check-3-in-a-row 0 2 1 0)))

; (defun diagonal-3-in-a-row ()
;   (or
;    (check-3-in-a-row 0 0 1 1)
;    (check-3-in-a-row 0 2 1 -1)))

; (defun check-3-in-a-row (row-start col-start row-offset col-offset)
;   (loop until (or
;                (>= row-start 3)
;                (>= col-start 3)
;                (< row-start 0)
;                (< col-start 0))
;         do (progn
;             (format t "~A~% ~A~% ~A~%" row-start col-start *current-player*)
;             (if (string= (get-cell row-start col-start) *current-player*)
;                 NIL
;                 (return-from check-3-in-a-row NIL))
;             (setq row-start (+ row-start row-offset))
;             (setq col-start (+ col-start col-offset))))
;   T)

;; for testing - (set-cell 0 0 "X") (set-cell 1 1 "X") (set-cell 2 2 "X")
(defun set-cell (row col cell)
  (setf (aref *game-board* row col) cell))

(defun get-cell (row col)
  (aref *game-board* row col))

;; (play-game)

;; tests

(defun test-diagonal ()
  (set-cell 0 0 "X")
  (set-cell 1 1 "X")
  (set-cell 2 2 "X")
  (diagonal-3-in-a-row))

(defun test-horizontal ()
  (set-cell 0 0 "X")
  (set-cell 0 1 "X")
  (set-cell 0 2 "X")
  (horizontal-3-in-a-row))

(defun test-X () (setf *game-board* #2A(("X" "X" "X")
                                        ("-" "O" "X")
                                        ("O" "-" "O")))
  (setf *current-player* "X")
  (horizontal-3-in-a-row))

(defun test-O () (setf *game-board* #2A(("X" "X" "X")
                                        ("-" "O" "X")
                                        ("O" "-" "O")))
  (setf *current-player* "O")
  (horizontal-3-in-a-row))

; (defun test-vertical ()
;   (set-cell 0 0 "X")
;   (set-cell 1 1 "X")
;   (set-cell 2 2 "X")
;   (vertica-3-in-a-row))