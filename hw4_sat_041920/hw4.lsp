(defun null-checker (delta)
  (if (= (length delta) 0) 
      t 
      nil)
)

(defun remove-element-function-parser (symbol clause fx-name)
	(cond ((string= fx-name "prune-element") 
						(if (null-checker clause) 
						    nil
						      (if (eq (car clause) symbol) 
						        (remove-element-function-parser symbol (cdr clause) "prune-element")
						        (append (list (car clause)) (remove-element-function-parser symbol (cdr clause) "prune-element"))
						      )
						  )
						)
		  ((string= fx-name "remove-element-top-level") 
		  				(if (null-checker clause) 
						    nil
						    (append (list (remove-element-function-parser symbol (car clause) "prune-element"))
						      		(remove-element-function-parser symbol (cdr clause)"remove-element-top-level"))
						 )
		  )
	)
)

(defun top_level_validity_parser (symbol clause type)
	(cond ((string= "top-level-valid-check" type) 
				(if (top_level_validity_parser symbol clause "clause-validity-check")
			    (remove-element-function-parser symbol clause "remove-element-top-level")
			    nil))
		  ((string= "clause-validity-check" type) 
		  		(if (null clause) 
	    			T
			    	(if (remove-element-function-parser symbol (car clause) "prune-element") 
			      		(top_level_validity_parser symbol (cdr clause) "clause-validity-check")
			      		nil)	
  				))
	)
)

(defun exists (symbol clause)
  (if (null-checker clause) 
    nil 
    (if (= (first clause) symbol) 
        t 
        (exists symbol (rest clause))
    )
  )
)

; Assemble output
(defun top-level-removal-fx (symbol clause output_clause)
  (if (null-checker clause) 
    output_clause
    (continue-removing clause symbol (car clause) (cdr clause) output_clause)
  )
)

(defun continue-removing (clause symbol clause_head clause_tail output_clause)
	(if (exists symbol clause_head)
      (top-level-removal-fx  symbol clause_tail output_clause)
      (top-level-removal-fx symbol clause_tail (append output_clause (list clause_head)))
    )
)

(defun process-branch (clause symbol)
  (cond ((post_removal_checks clause symbol) nil)
      ((null (top-level-removal-fx symbol clause nil)) symbol)
      ((and (equal (top-level-removal-fx symbol clause nil) clause) (equal (top-level-removal-fx (- symbol) clause nil) clause)) clause)
      (t (top_level_validity_parser (- symbol) (top-level-removal-fx symbol clause nil) "top-level-valid-check"))  
  )
)

(defun post_removal_checks (clause symbol)
  (if (or (or (null (top_level_validity_parser (- symbol) clause "top-level-valid-check")) 
      (equal (top-level-removal-fx symbol clause nil) clause))
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
  