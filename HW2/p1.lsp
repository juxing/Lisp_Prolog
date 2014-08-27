;
; p1.lsp:    A Permutation Product Generator
;
;      - reads lists of symbols from p1.dat of the form:
;               ( (a b c) ... (y z) )
;      - produces all permutation products of each symbol set
;      - invoked via:  (p1)


;
; Top level control loop:
;
(defun p1 ()          

    (let (dat_file)

	(setf dat_file   (open "/users/kdejong/kdj/cs580/homework/HW1/p1.dat" :direction :input))
;	(setf dat_file   (open "p1.dat" :direction :input))
	(setf *print_max* 10)     ; not declared in a LET => global variable

	(loop 
	    (setf sym_list (read dat_file nil nil))
	    (if (null sym_list)	   (return))
	    (gen_perm sym_list)
	    )

	(close dat_file)
	'***ALL_DONE***
    )
)


(defun gen_perm (sym_list)

    (format t "~%Generating permutation products of: ~A~%" sym_list)

    (setf *leaf_cnt* 0)
    (setf *node_cnt* 0)
    (permute (rest sym_list) (first sym_list) '() '())

    (format t "~%  Node count: ~D" *node_cnt*)
    (format t "~%  Leaf count: ~D~%~%" *leaf_cnt*)
    )


;
; permute is invoked with four args: 
;     (unused sets) (unused symbols) (partial permutation) (partial product)
;
; If the unused symbol list is null, the partial permutation is a complete 
; one and ready to be added to the partial product;
;
; If there are no more used sets, then the recursion has bottomed out and
; the partial product is complete and ready to be printed.
;
; If there are unused symbols, each in turn added to the front of the partial
; permutation, creating a child node, and permute is recursively invoked
; to generate the grandchildren.

(defun permute (unused_sets unused_sym perm perm_prod)

  (let ()

    (setf *node_cnt* (1+ *node_cnt*))
    (cond
	 ( (null unused_sym)       ; have a completed permutation to add to product

	      (setq perm_prod (cons perm perm_prod))

	      (cond
		   ( (null unused_sets)    ; bottomed out at leaf node

			(cond  
		             ( (< *leaf_cnt* *print_max*)   
			             (print (reverse perm_prod)) )
		             ( (= *leaf_cnt* *print_max*)   
			             (format t "~%       suppressing printing ...") )
			     )
		        (setf *leaf_cnt* (1+ *leaf_cnt*))
			)

		   ( t                    ; get next set & permute it
		        (permute (rest unused_sets) (first unused_sets) '() perm_prod))
		   )
	      )

	 ( t                           ; generate children
	      (dolist (item unused_sym)
	         (permute  unused_sets (remove item unused_sym) (cons item perm) perm_prod) )
	      )
	 )
    )
  )