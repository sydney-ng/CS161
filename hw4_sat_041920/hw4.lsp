; checks if what we are passing in still has anything to check
; is used as a helper fx for many of the other fx
(defun null-checker (delta)
  (if (= (length delta) 0) 
      t 
      nil)
)

; this is the parent function that will remove elements from the clauses as we parse them 
(defun discard-symbol-function-parser (symbol clause fx-name)
	(if (null-checker clause)
		nil
		(cond ((string= fx-name "prune-element") ; tries to prune symbol from the clause 
		      (if (eq (car clause) symbol) ; match with first element
		        (discard-symbol-function-parser symbol (cdr clause) "prune-element") 
		        ; then remove it and continue with rest 
		        (append (list (car clause)) (discard-symbol-function-parser symbol (cdr clause) "prune-element"))
		      	; else keep and then do rest 
		      ))
							
			  ((string= fx-name "discard-symbol-top-level") ; going through clauses 
			    (append (list (discard-symbol-function-parser symbol (car clause) "prune-element")) ; call the sub function 
			    (discard-symbol-function-parser symbol (cdr clause) "discard-symbol-top-level"))) ; plus do it for the rest of the clauses 
		)
	)
)

; checks to make sure the clause is still valid post-discarding
(defun top_level_validity_parser (symbol clause type)
	(cond ((string= "top-level-valid-check" type) 
				(if (top_level_validity_parser symbol clause "clause-validity-check")
				; need to go one level deeper into clauses 
			    (discard-symbol-function-parser symbol clause "discard-symbol-top-level")
			    ; do the discarding 
			    nil))
		  ((string= "clause-validity-check" type) 
		  		(if (null clause) 
	    			T
			    	(if (discard-symbol-function-parser symbol (car clause) "prune-element") ; if you can prune the element 
			      		(top_level_validity_parser symbol (cdr clause) "clause-validity-check") 
			      		; keep checking for the rest to see if the rest of clauses also still will be valid 
			      		nil)	
  				))
	)
)

; checks -> does a symbol exist in this clause? 
(defun exists (symbol clause)
  (if (null-checker clause) ; null clause 
    nil 
    (if (= (first clause) symbol) ; found symbol 
        t 
        (exists symbol (rest clause)) ; didn't find symbol, keep checking 
    )
  )
)

; Assemble output
(defun top-level-removal-fx (symbol clause output_clause)
  (if (null-checker clause) 
    output_clause
    (continue-removing symbol clause (car clause) (cdr clause) output_clause)
  )
)

(defun continue-removing (symbol clause clause_head clause_tail output_clause)
	(if (exists symbol clause_head)
      (top-level-removal-fx  symbol clause_tail output_clause)
      (top-level-removal-fx symbol clause_tail (append output_clause (list clause_head)))
    )
)

(defun iterate-through-branch (symbol clause)
  (if (confirm_branch_validity symbol clause) 
  		nil
  		(if (null (top-level-removal-fx symbol clause nil))
  			symbol
  			(begin-removal symbol (- symbol) clause 
  						    (top-level-removal-fx symbol clause nil) 
  						    (top-level-removal-fx (- symbol) clause nil))
  		)
  )
)

(defun begin-removal (symbol neg_symbol clause remove-pos-sym remove-neg-sym)
	 (if (equal remove-pos-sym clause) 
	 	 (if (equal remove-neg-sym clause)
	 	 	clause
	 	 	(top_level_validity_parser neg_symbol remove-pos-sym "top-level-valid-check")
	 	 ) 
         (top_level_validity_parser neg_symbol remove-pos-sym "top-level-valid-check")
      )  
)


(defun confirm_branch_validity (symbol clause)
  (if (or (or (null (top_level_validity_parser (- symbol) clause "top-level-valid-check")) 
      (equal (top-level-removal-fx symbol clause nil) clause))
      (null-checker clause)
    )
      t 
      nil
  )
)

(defun SAT-parser-prechecks (clause symbol n)
  (if (or (null clause) (> symbol n))
    t
    nil)
) 

(defun answer-generator (cur_sol vars_remaining)
  (if (= vars_remaining 0) 
    cur_sol
    (answer-generator (append cur_sol (list (+ (length cur_sol) 1))) (- vars_remaining 1))
  )
)

(defun SAT-parser (clause symbol n)
    (cond ((SAT-parser-prechecks clause symbol n) nil)

        ;We've reached a single clause
        ((atom clause) (list clause))
        ;Process the choice of taking T and the choice of taking F
        (t (go-through-branches clause symbol n (iterate-through-branch symbol clause) (iterate-through-branch (- symbol) clause)))
    )
)

(defun prune-branch (symbol n T_branch temp_T_val)
  (cond ((or (null T_branch) (null temp_T_val)) nil)
        ((atom T_branch) (list symbol))
        (t (append (list symbol) temp_T_val))
      )
)

(defun null-branch-parse (temp_T_val T_branch symbol)
  (cond ((null T_branch) nil)
      (t (cond ((atom T_branch) (list symbol))
             (t (cond ((null temp_T_val) nil)
                  (t (append (list symbol) temp_T_val))
                )
             )

          )
      )
  )
)

(defun go-through-branches (clause symbol n T_branch F_branch)
    (if (null F_branch)
      (null-branch-parse (SAT-parser T_branch (+ symbol 1) n) T_branch symbol) 
      (null-branch-parse2 F_branch (SAT-parser F_branch (+ symbol 1) n) (SAT-parser T_branch (+ symbol 1) n)symbol)
         
  )
)

(defun null-branch-parse2 (F_branch temp_F_val temp_T_val symbol)
  (cond ((atom F_branch)(list (- symbol)))
  		(t (cond ((null temp_F_val) (cond ((null temp_T_val) nil)
  									 (t (append (list symbol) temp_T_val)))
  								) 
  				  (t (append (list (- symbol)) temp_F_val))
  		   )
  		)
  )
)

(defun sat? (n delta)
	(if (= n 0) 
		nil 
		(solve-sat? n 1 delta)
	)
)

(defun solve-sat? (n num delta)
	(do-work (SAT-parser delta num n) n)
)

(defun do-work (sol_so_far n)
	(if (null-checker sol_so_far)
		nil 
		(do-work-2 sol_so_far (- n (length sol_so_far)))
	)       

)

(defun do-work-2 (sol_so_far num_still_to_parse)
	(answer-generator sol_so_far num_still_to_parse)
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
  