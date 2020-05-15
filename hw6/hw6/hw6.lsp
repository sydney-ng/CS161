;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;

(defun reload()
  (load "hw6.lsp")
  );end defun


(defun nil_checker (k c)
    (if (< k c) t nil)
) 

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
    (+ (* (- n 1) k) c) ; self explanatory (does what is in the function definition) 
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;

(defun at-least-one-color (n c k)
  (if (nil_checker k c) ; is color index less than max? 
      nil ; return false if it does 
      (if (= c k) 
          (list (node2var n k k)) ; done -> return last index 
          (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)) ; get index, go onto next 
      )

  )
)

(defun negate (val)
   (* -1 val)
)

(defun make_list (x y)
  (list (list x y))
) 

(defun return_conjunction (n k)
  (let (x (* n k)) (make_list x (negate x)))
)

(defun at-most-one-color (n c k)
    (if (nil_checker k c)
        nil
        (continue-at-most-one-color n c k (at-least-one-color n c k)) 
    )
)

(defun continue-at-most-one-color (n c k clause)
  (if (equal k c) 
      (return_conjunction n k)
      (get-one-at-most clause)
  )
)

(defun get-one-at-most (s)
  (if (nil_checker (length s) 1)
      nil 
      (append (separate_clause_before_parsing s) 
               (get-one-at-most (cdr s)))
  )   
)

(defun separate_clause_before_parsing (s)
  (let* ((neg_c_head (negate (car s))) 
         (c_rest (cdr s))) 
    (permutation-one-at-most neg_c_head c_rest)
  ) 
) 

(defun permutation-one-at-most(a b)
  (if (nil_checker (length b) 1) 
      nil
     (append (make_list a (negate (car b)))
             (permutation-one-at-most a (cdr b)))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
(defun generate-node-clauses (n k)
    (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k))
)
; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;


(defun format_generate_edge_clauses_p1 (e c k)

  (let* ((node_to_var_first (* (node2var (first e) c k) -1)) 
         (node_to_var_sec (* (node2var (second e) c k) -1)))
    (make_list node_to_var_first node_to_var_sec)
 )
) 

(defun child-fx-generate-edge-clause (e c k)
  (if 
    (nil_checker k c) 
      nil
    (append (format_generate_edge_clauses_p1 e c k) (child-fx-generate-edge-clause e (+ c 1) k))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
  (child-fx-generate-edge-clause e 1 k)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
         (+ node 1)
         ))
  ((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
         (read-line in nil 'eof)))
  ((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
  (setq cnf (append (generate-edge-clauses edge k) cnf))
  );end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
       )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
  (t (progn 
       (format out "~A " (car clause))
       (write-clause-to-file out (cdr clause))
       );end progn
     );end t
  );end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun