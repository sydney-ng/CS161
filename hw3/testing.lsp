(setq p1 '((0 0 1 1 1 1 0 0 0 ) (1 1 1 0 0 1 1 1 1) (1 1 1 0 0 1 1 1 1) (1 1 1 0 0 1 1 1 1) (1 1 1 0 0 1 1 1 1)
(1 1 1 1 1 1 1 1 1)))
;(defun test_list (test)
;	 (length (car test))
;)

(defun goal-test (s)
  (cond ((equal (length s) 1) (check_list_goal s)) 
  		(t (and (check_list_goal (first s)) (goal-test (rest s)))) ; split and check the rest
);end defun
  )

(defun check_list_goal (s)
  (cond ((equal (length s) 1) t) 
  (t (and (check_element_goal (first s)) (check_list_goal (rest s)))) ; split and check the rest
  )
)

(defun check_element_goal (s)
  (cond ((isBlank s)t) ; blank = ok 
  		((isWall s) t) ; wall is OK 
  		((isBox s) nil) ; box = not OK 
  		((isKeeper s) nil) ; keeper = not OK 
  		((isStar s) t) ; star = OK 
  		((isKeeperStar s) t) ; keeper on goal = OK 
  		((isBoxStar s) t) ; box on goal = OK
  		(t nil) ; catch any other cases 
  )
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