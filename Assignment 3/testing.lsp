(setq p0 '((1 0 1 1 1 1 0 0 0 ) (1 1 1 0 0 1 1 1 1) (1 1 4 0 0 1 1 1 1) (1 1 1 0 0 1 1 1 1) (1 1 1 0 0 1 1 1 1)
(1 1 1 1 1 1 1 1 1)))


(defun goal-test (s)
  (cond ((> (length s) 1) (and (check_list_goal (car s)) (goal-test (cdr s))))
        (t(check_list_goal (car s)))
  )
)

(defun check_element_goal (s)
  (cond ((isBlank s) t) ; blank = ok 
  		((isWall s) t) ; wall is OK 
  		((isBox s) nil) ; box = not OK 
  		((isKeeper s) nil) ; keeper = not OK 
  		((isStar s) t) ; star = OK 
  		((isKeeperStar s) t) ; keeper on goal = OK 
  		((isBoxStar s) t) ; box on goal = OK
  		(t nil) ; catch any other cases 
  )
)

(defun check_list_goal (s)
  (cond  ((> (length s) 1) (and (check_element_goal (car s)) (check_list_goal (cdr s)))) ;check if the last number is 2
    (t (check_element_goal (car s)))) ;check if the first element is 2, then run on rest of list
)

(defun h1 (s)
   (cond ((> (length s) 1) (+ (helper_counter_1 (car s)) (h1 (cdr s))))
         (t(helper_counter_1 (car s)))
))

(defun helper_counter_1 (s)
  (cond  ((> (length s) 1) (+ (helper_counter_2 (car s)) (helper_counter_1 (cdr s)))) ;check if the last number is 2
    (t (helper_counter_2 (car s)))) ;check if the first element is 2, then run on rest of list
)

(defun helper_counter_2 (s)
  (cond ((isBox s) 1)
    (t 0)
)
  )


(defun get-square (s r c)
  (cond ((and (= r 0) (= c 0)) (caar s)) ; return the value, found correct row/col
      ((> r (- (length s) 1)) wall)
      ((> c (- (length (first s)) 1)) wall)
      ((not (= r 0)) (get-square (cdr s) (- r 1) c)) ; incorrect row, iterate
      ((not (= c 0)) (get-square (cons (cdar s) (cdr s)) r (- c 1))) ;get rid of the first c atoms in the first inner list
      (t wall)
  )
)

;;;;;;;;
; parameters state s, row r, col c, square content v 
(defun set-square (s r c v)
    (cond ((null s) nil) ; empty state 
          ((not (= r 0)) (cons (car s) (set-square (cdr s) (- r 1) c v))) ; incorrect row right now 
          ((not (= c 0)) (cons (cons (caar s) (car (set-square (cons (cdar s) (cdr s)) r (- c 1) v)) ) (cdr s))) ; incorrect col right now 
          ((and (= r 0) (= c 0)) (cons (cons v (cdar s)) (cdr s))) ; found right r/c 
          (t nil) ; just in case we missed anything 
  )
)

; return the state that is the result of moving the keeper in state S in direction D
; nil if move = invalid 
(defun try-move1 (s d)
        (cond ((string= d "left") (move_left s))
              ((string= d "right") (move_right s))
              ((string= d "up") (move_up s))
              ((string= d "down") (move_down s))
        ) ; end cond 
      ) ; end let binding 

;;;;;;;;;;;;;;;;;;;;;;; right helper fx 

(defun move_right (s)
  (let* ((r (second (getKeeperPosition s 0))) (c (first (getKeeperPosition s 0))))
    (cond ((isWall (get-square s r (+ c 1))) nil) 
          ((and (isBox (get-square s r (+ c 1))) (or (isBox (get-square s r (+ c 2))) (isWall (get-square s r (+ c 2))))) nil) 
          (t (if (isKeeperStar (get-square s r c)) (set-square (move_keeper__right s r c) r c star) (set-square (move_keeper__right s r c) r c blank)))
    )
  )
) ; end fx 

(defun move_keeper__right (s r c)
  (if (confirm_keeper_value1 s r (+ c 1)) (set-square (move_block__left s r c) r (+ c 1) keeperstar) (set-square (move_block__right s r c) r (+ c 1) keeper))
)

(defun move_block__right (s r c)
(cond ((confirm_box_value1 s r (+ c 1)) (set-square s r (+ c 2) box)) ;move the block if applicable
        (t s)))

;;;;;;;;;;;;;;;;;;;;;;; left helper fx 

(defun move_left (s)
  (let* ((r (second (getKeeperPosition s 0))) (c (first (getKeeperPosition s 0))))
    (cond ((isWall (get-square s r (- c 1))) nil) 
          ((and (isBox (get-square s r (- c 1))) (or (isBox (get-square s r (- c 2))) (isWall (get-square s r (- c 2))))) nil) 
          (t (if (isKeeperStar (get-square s r c)) (set-square (move_keeper__left s r c) r c star) (set-square (move_keeper__left s r c) r c blank)))
    )
  )
) 

(defun move_keeper__left (s r c)
  (if (confirm_keeper_value1 s r (- c 1)) (set-square (move_block__left s r c) r (- c 1) keeperstar) (set-square (move_block__left s r c) r (- c 1) keeper))
)

(defun move_block__left (s r c)
(cond ((confirm_box_value1 s r (- c 1)) (set-square s r (- c 2) box)) ;move the block if applicable
        (t s)))

;;;;;;;;;;;;;;;;;;;;;;; up helper fx 

(defun move_up (s)
  (let* ((r (second (getKeeperPosition s 0))) (c (first (getKeeperPosition s 0))))
    (cond ((isWall (get-square s (- r 1) c)) nil) 
          ((and (isBox (get-square s (- r 1) c)) (or (isBox (get-square s (- r 2) c)) (isWall (get-square s (- r 2) c)))) nil) 
          (t (if (isKeeperStar (get-square s r c)) (set-square (move_keeper__up s r c) r c star) (set-square (move_keeper__up s r c) r c blank)))
    )
  )
) ; end fx 

(defun move_keeper__up (s r c)
  (if (confirm_keeper_value1 s (- r 1) c) (set-square (move_block__up s r c) (- r 1) c keeperstar) (set-square (move_block__up s r c) (- r 1) c keeper))
)

(defun move_block__up (s r c)
(cond ((confirm_box_value1 s (- r 1) c) (set-square s (- r 2) c box)) ;move the block if applicable
        (t s)))

;;;;;;;;;;;;;;;;;;;;;;; down helper fx 

(defun move_down (s)
  (let* ((r (second (getKeeperPosition s 0))) (c (first (getKeeperPosition s 0))))
    (cond ((isWall (get-square s (+ r 1) c)) nil) 
          ((and (isBox (get-square s (+ r 1) c)) (or (isBox (get-square s (+ r 2) c)) (isWall (get-square s (+ r 2) c)))) nil) 
          (t (if (isKeeperStar (get-square s r c)) (set-square (move_keeper__down s r c) r c star) (set-square (move_keeper__down s r c) r c blank)))
    )
  )
) ; end fx 

(defun move_keeper__down (s r c)
  (if (confirm_keeper_value1 s (+ r 1) c) (set-square (move_block__down s r c) (+ r 1) c keeperstar) (set-square (move_block__down s r c) (+ r 1) c keeper))
)

(defun move_block__down (s r c)
(cond ((confirm_box_value1 s (+ r 1) c) (set-square s (+ r 2) c box)) ;move the block if applicable
        (t s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun confirm_box_value1 (s r c)
  (cond ((isBox (get-square s r c)) t) 
        ((isBoxStar (get-square s r c)) t) 
        (t nil))
)

(defun confirm_keeper_value1 (s r c)
  (cond ((isStar (get-square s r c)) t) 
        ((isBoxStar (get-square s r c)) t) 
        (t nil))
)


(defun moveKeeper (state row column)
  (cond 
    ((null state) nil)
    (t
      (let* (
          (position (getKeeperPosition state 0))
          (x (car position))
          (y (cadr position))
          (keeperOld (cond ((isKeeper (get-square state y x)) blank) (t star)))
          (keeperNew 
            (cond
              ((isStar (get-square state row column)) keeperstar)
              (t keeper)
            )
          )
        )
        (set-square (set-square state row column keeperNew) y x keeperOld)
      )
    )
  )
)


(defun getKeeperColumn (r col)
  (cond ((null r) nil)
  (t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
         col
       (getKeeperColumn (cdr r) (+ col 1))
       );end if
     );end t
  );end cond
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
  (t (let ((x (getKeeperColumn (car s) 0)))
       (if x
     ;keeper is in this row
     (list x row)
     ;otherwise move on
     (getKeeperPosition (cdr s) (+ row 1))
     );end if
         );end let
   );end t
  );end cond
  );end defun


(defun maxRow (s)
  (- (length s) 1) ;return max index of rows
)

(defun maxCol (s)
  (- (length (first s)) 1) ;return max index of column
)
; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )


(setq p1 '((12 4 15 6 7 8)
     (2 4 5 6 7 8)
     (2 4 5 6 7 8)
    (2 4 5 6 7 8)
     (2 4 5 6 7 8)
    (2 4 5 6 7 8)
     (2 4 5 6 7 8)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
     (1 0 0 0 0 0 1) 
     (1 0 0 0 0 0 1) 
     (1 0 0 2 1 4 1) 
     (1 3 4 0 1 0 1)
     (1 1 1 1 1 1 1)))

(setq p22 '((1 1 1 1 1 1 1)
     (1 0 0 0 0 0 1) 
     (1 0 0 3 0 0 1) 
     (1 0 0 0 1 4 1) 
     (1 1 4 2 1 0 1)
     (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 0 0 2 0 3 4 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 4 0 1 0 0 0 1)
     (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
     (0 0 0 0 0 1 4)
     (0 0 0 0 0 0 0)
     (0 0 1 1 1 0 0)
     (0 0 1 0 0 0 0)
     (0 2 1 0 0 4 0)
     (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
     (1 1 0 0 1 1)
     (1 0 0 0 0 1)
     (1 4 2 2 4 1)
     (1 0 0 0 4 1)
     (1 1 3 1 1 1)
     (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
     (1 0 0 0 0 0 4 1)
     (1 4 0 0 2 2 3 1)
     (1 0 0 1 0 0 4 1)
     (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
     (0 0 1 1 1 1 4 0 0 3)
     (0 0 0 0 0 1 0 0 0 0)
     (0 0 0 0 0 1 0 0 1 0)
     (0 0 1 0 0 1 0 0 1 0)
     (0 2 1 0 0 0 0 0 1 0)
     (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
     (1 4 0 0 4 1)
     (1 0 2 2 0 1)
     (1 2 0 1 0 1)
     (1 3 4 0 4 1)
     (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
     (1 1 1 0 0 1 1 1 1) 
     (1 0 0 0 0 0 2 0 1) 
     (1 0 1 0 0 1 2 0 1) 
     (1 0 4 4 4 1 3 0 1) 
     (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
      (1 4 0 0 1 1 0)
      (1 3 2 0 0 1 1)
      (1 1 0 2 0 0 1)
      (0 1 1 0 2 0 1)
      (0 0 1 1 0 0 1)
      (0 0 0 1 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
      (0 2 1 4 0 4 0)
      (0 2 0 4 0 0 0)    
      (3 2 1 1 1 4 0)
      (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
      (1 0 0 4 1 0 0 0)
      (1 2 1 0 1 1 1 1)
      (1 4 0 0 0 0 0 1)
      (1 0 0 5 0 5 0 1)
      (1 0 5 0 1 0 1 1)
      (1 1 1 0 3 0 1 0)
      (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
      (1 3 0 0 1 0 0 4 4 1)
      (1 0 2 0 2 0 0 4 4 1)
      (1 0 2 2 2 1 1 4 4 1)
      (1 0 0 0 0 1 1 4 4 1)
      (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
      (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)     
      (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
      ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
      (1 1 1 0 0 1 1 1 1 0)
      (1 0 0 2 0 0 0 1 1 0)
      (1 3 2 0 2 0 0 0 1 0)
      (1 1 0 2 0 2 0 0 1 0)
      (0 1 1 0 2 0 2 0 1 0)
      (0 0 1 1 0 2 4 0 1 0)
      (0 0 0 1 1 1 1 0 1 0)
      (0 0 0 0 1 4 1 0 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 0 1 4 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 1 1 1 1 1)))