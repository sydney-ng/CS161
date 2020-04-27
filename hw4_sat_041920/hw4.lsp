(defun null-checker (delta)
  (if (= (length delta) 0) 
      t 
      nil)
)

(defun remove-element-function-parser (symbol clause fx-name)
	(cond ((string= fx-name "remove-element") 
						(if (null-checker clause) 
						    nil
						      (if (eq (car clause) symbol) 
						        (remove-element-function-parser symbol (cdr clause) "remove-element")
						        (append (list (car clause)) (remove-element-function-parser symbol (cdr clause) "remove-element"))
						      )
						  )
						)
		  ((string= fx-name "remove-element-top-level") 
		  				(if (null-checker clause) 
						    nil
						    (append 
						      (list (remove-element-function-parser symbol (first clause) "remove-element"))
						      (remove-element-function-parser symbol (rest clause)"remove-element-top-level"))
						 )
		  )
	)
)

(defun is-valid-clause (symbol clause)
  (if (null clause) 
    T
    (if (remove-element-function-parser symbol (first clause) "remove-element") 
      (is-valid-clause symbol (rest clause))
      nil 
    )
  )
) 

(defun is-valid-removal (clause symbol)
  (if (is-valid-clause symbol clause)
    (remove-element-function-parser symbol clause "remove-element-top-level")
    nil
  )
)

(defun is-in-list (symbol clause)
  (if (null-checker clause) 
    nil 
    (if (= (first clause) symbol) 
        t 
        (is-in-list symbol (rest clause))
    )
  )
)

; Assemble output
(defun remove-with-varh1 (clause symbol output_clause)
  (if (null-checker clause) 
    output_clause
    (if (is-in-list symbol (first clause))
      (remove-with-varh1 (rest clause) symbol output_clause)
      (remove-with-varh1 (rest clause) symbol (append output_clause (list (first clause))))
    )
  )
)



(defun remove-with-var (clause symbol)
  (remove-with-varh1 clause symbol nil)
)


(defun process-branch (clause symbol)
  (cond ((post_removal_checks clause symbol) nil)
      ((null (remove-with-var clause symbol)) symbol)
      ((and (equal (remove-with-var clause symbol) clause) (equal (remove-with-var clause (- symbol)) clause)) clause)
      (t (is-valid-removal (remove-with-var clause symbol) (- symbol)))  
  )
)

(defun post_removal_checks (clause symbol)
  (if (or (or (null (is-valid-removal clause (- symbol))) 
      (equal (remove-with-var clause symbol) clause))
      (null-checker clause)
    )
      t 
      nil
  )
)

(defun smart-DFS-prechecks (clause symbol n)
  (if (or (null clause) (> symbol n))
    t
    nil)
) 

(defun smart-DFS-mine (clause symbol n)
  (cond ((smart-DFS-prechecks clause symbol n) nil)
      ;We've reached a single clause
      ((atom clause) (list clause))
      ;Process the choice of taking T and the choice of taking F
        (t ( clause symbol n (process-branch clause symbol) (process-branch clause (- symbol))))
  )
) 

(defun check_branches (clause symbol n T_branch F_branch)
  (cond ((null F_branch) (F_branch_parse symbol (smart-DFS T_branch (+ symbol 1) n) T_branch)) 
      (t (T_branch_parse T_branch F_branch symbol (smart-DFS F_branch (+ symbol 1) n) (smart-DFS T_branch (+ symbol 1) n)))
  )
)

(defun F_branch_parse (symbol assnT T_branch)
  (cond ((or (null T_branch) (null assnT)) NIL) 
      ((atom T_branch) (list symbol))
      (t (append (list symbol) assnT))
  ) 
)

(defun T_branch_parse (T_branch F_branch symbol assnF assnT)
  (cond ((atom F_branch) (list (- symbol)))
      ((null assnF) (if (null assnT) 
                  NIL
                  (append (list symbol) assnT))
              )
      (t (append (list (- symbol)) assnF))
  )
)


(defun populate-sol (cur_sol vars_remaining)
  (if (= vars_remaining 0) 
    cur_sol
    (populate-sol (append cur_sol (list (+ (length cur_sol) 1))) (- vars_remaining 1))
  )
)




(defun smart-DFS (clause symbol n)
    (cond ((smart-DFS-prechecks clause symbol n) nil)

        ;We've reached a single clause
        ((atom clause) (list clause))
        ;Process the choice of taking T and the choice of taking F
        (t (go-through-branches clause symbol n (process-branch clause symbol) (process-branch clause (- symbol))))
    )
)

(defun prune-branch (symbol n T_branch assnT)
  (cond ((or (null T_branch) (null assnT)) nil)
        ((atom T_branch) (list symbol))
      (t (append (list symbol) assnT))
      )
)

(defun null-branch-parse (assnT T_branch symbol)
  (cond ((null T_branch) nil)
      (t (cond ((atom T_branch) (list symbol))
             (t (cond ((null assnT) nil)
                  (t (append (list symbol) assnT))
                )
             )

          )
      )
  )
)

(defun go-through-branches (clause symbol n T_branch F_branch)
    (if (null F_branch)
      (null-branch-parse (smart-DFS T_branch (+ symbol 1) n) T_branch symbol) 
      (null-branch-parse2 F_branch (smart-DFS F_branch (+ symbol 1) n) (smart-DFS T_branch (+ symbol 1) n)symbol)
         
  )
)

(defun null-branch-parse2 (F_branch assnF assnT symbol)
  (cond ((atom F_branch)(list (- symbol)))
  		(t (cond ((null assnF) (cond ((null assnT) nil)
  									 (t (append (list symbol) assnT)))
  								) 
  				  (t (append (list (- symbol)) assnF))
  		   )
  		)
  )
)

(defun sat? (n delta)
	(if (= n 0) 
		nil 
		(process-solution n 1 delta)
	)
)

(defun process-solution (n init delta)
	(do-work (smart-DFS delta init n) n)
)

(defun do-work (pre_proc_sol n)
	
	(if (null-checker pre_proc_sol)
		nil 
		(do-work-2 pre_proc_sol (- n (length pre_proc_sol)))
	)       

)

(defun do-work-2 (pre_proc_sol num_remaining)
	(populate-sol pre_proc_sol num_remaining)
)

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))
  