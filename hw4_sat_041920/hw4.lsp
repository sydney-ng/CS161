(defun remove-element (symbol clause)
  (if (null-checker clause) 
    nil
      (if (eq (car clause) symbol) 
        (remove-element symbol (cdr clause))
        (append (list (car clause)) (remove-element symbol (cdr clause)))
      )
  )
)
          

(defun null-checker (delta)
  (if (= (length delta) 0) 
      t 
      nil)
)


(defun remove-elem-all (clause val)
  (if (null-checker clause) 
    nil
    (append 
      (list (remove-element val (first clause)))
      (remove-elem-all (rest clause) val))
  )
)


(defun is-valid-cnf (cnf cur_var)
  (if (null cnf) 
    T
    (if (remove-element cur_var (first cnf)) 
      (is-valid-cnf (rest cnf) cur_var)
      nil 
    )
  )
) 

(defun is-valid-removal (cnf cur_var)
  (if (is-valid-cnf cnf cur_var)
    (remove-elem-all cnf cur_var)
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
(defun remove-with-varh1 (cnf cur_var output_cnf)
  (if (null-checker cnf) 
    output_cnf
    (if (is-in-list cur_var (first cnf))
      (remove-with-varh1 (rest cnf) cur_var output_cnf)
      (remove-with-varh1 (rest cnf) cur_var (append output_cnf (list (first cnf))))
    )
  )
)


(defun remove-with-var (cnf cur_var)
  (remove-with-varh1 cnf cur_var NIL)
)


(defun process-branch (cnf cur_var)
  (cond ((post_removal_checks cnf cur_var) nil)
      ((null (remove-with-var cnf cur_var)) cur_var)
      ((and (equal (remove-with-var cnf cur_var) cnf) (equal (remove-with-var cnf (- cur_var)) cnf)) cnf)
      (t (is-valid-removal (remove-with-var cnf cur_var) (- cur_var)))  
  )
)

(defun post_removal_checks (cnf cur_var)
  (if (or (or (null (is-valid-removal cnf (- cur_var))) 
      (equal (remove-with-var cnf cur_var) cnf))
      (null-checker cnf)
    )
      t 
      nil
  )
)

(defun smart-DFS-prechecks (cnf cur_var n)
  (if (or (null cnf) (> cur_var n))
    t
    nil)
) 

(defun smart-DFS-mine (cnf cur_var n)
  (cond ((smart-DFS-prechecks cnf cur_var n) nil)
      ;We've reached a single clause
      ((atom cnf) (list cnf))
      ;Process the choice of taking T and the choice of taking F
        (t ( cnf cur_var n (process-branch cnf cur_var) (process-branch cnf (- cur_var))))
  )
) 

(defun check_branches (cnf cur_var n T_branch F_branch)
  (cond ((null F_branch) (F_branch_parse cur_var (smart-DFS T_branch (+ cur_var 1) n) T_branch)) 
      (t (T_branch_parse T_branch F_branch cur_var (smart-DFS F_branch (+ cur_var 1) n) (smart-DFS T_branch (+ cur_var 1) n)))
  )
)

(defun F_branch_parse (cur_var assnT T_branch)
  (cond ((or (null T_branch) (null assnT)) NIL) 
      ((atom T_branch) (list cur_var))
      (t (append (list cur_var) assnT))
  ) 
)

(defun T_branch_parse (T_branch F_branch cur_var assnF assnT)
  (cond ((atom F_branch) (list (- cur_var)))
      ((null assnF) (if (null assnT) 
                  NIL
                  (append (list cur_var) assnT))
              )
      (t (append (list (- cur_var)) assnF))
  )
)


(defun populate-sol (cur_sol vars_remaining)
  (if (= vars_remaining 0) 
    cur_sol
    (populate-sol (append cur_sol (list (+ (length cur_sol) 1))) (- vars_remaining 1))
  )
)




(defun smart-DFS (cnf cur_var n)
    (cond ((smart-DFS-prechecks cnf cur_var n) nil)

        ;We've reached a single clause
        ((atom cnf) (list cnf))
        ;Process the choice of taking T and the choice of taking F
        (t (go-through-branches cnf cur_var n (process-branch cnf cur_var) (process-branch cnf (- cur_var))))
    )
)

(defun prune-branch (cur_var n T_branch assnT)
  (cond ((or (null T_branch) (null assnT)) nil)
        ((atom T_branch) (list cur_var))
      (t (append (list cur_var) assnT))
      )
)

(defun null-branch-parse (assnT T_branch cur_var)
  (cond ((null T_branch) nil)
      (t (cond ((atom T_branch) (list cur_var))
             (t (cond ((null assnT) nil)
                  (t (append (list cur_var) assnT))
                )
             )

          )
      )
  )
)

(defun go-through-branches (cnf cur_var n T_branch F_branch)
    (if (null F_branch)
      (null-branch-parse (smart-DFS T_branch (+ cur_var 1) n) T_branch cur_var) 
      (null-branch-parse2 F_branch (smart-DFS F_branch (+ cur_var 1) n) (smart-DFS T_branch (+ cur_var 1) n)cur_var)
         
  )
)

(defun null-branch-parse2 (F_branch assnF assnT cur_var)
  (cond ((atom F_branch)(list (- cur_var)))
  		(t (cond ((null assnF) (cond ((null assnT) nil)
  									 (t (append (list cur_var) assnT)))
  								) 
  				  (t (append (list (- cur_var)) assnF))
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
	(let* ((num_remaining (- n (length pre_proc_sol))))
		(if (null-checker pre_proc_sol)
			nil 
			(populate-sol pre_proc_sol num_remaining)
		)        
	)

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

  