; Ming Zhang
; cs580
; Homework #1

; p1.lsp:    A Permutation Product Generator
;
;      - reads lists of lists of symbols from p1.dat
;      - produces all permutations of each symbol set
;      - produces products among all the permutations
;      - invoked via:  (p0)
;

;
; Top level control loop:
;
(defun p1 ()          
    (let (dat_file)
	(setf dat_file   (open "p1.dat" :direction :input))
	(setf *print_max* 30)     ; not declared in a LET => global variable
        (setf *print_n* 0)

	(loop 
	    (setq *sym_list* (read dat_file nil nil))  ;  Input is a list of lists of symbols.
	    (if (null *sym_list*)	   (return))

            (setf *current* (make-array (length *sym_list*)))  ; An imtermedian array to store current product of permutations.
            (permute_products (elt *sym_list* 0) '() 0)
            (setf *print_n* 0)  ; After produce permutation products of one list of lists of symbols, reset to 0.
	    )

	(close dat_file)
	'***ALL_DONE***
    )
)

;  This func will permute each list of symbols, put one permutation into *current*, then 
;  permute the next list of symbols, when reach the end of input, then print out *current*,
;  which contains an array of one production of permutation.
(defun permute_products (unused_syms partial_perm k)  ;  k is used to track which list are we permuting now.
    (cond 
        ((null unused_syms)  
            (setf (aref *current* k) partial_perm)  ;  When get a permutation, put it into *current*.
            (cond
                ((= k (- (length *sym_list*) 1))  ;  If this permutation is of the last list of input, we print out *current*.
                    (cond
                        ((< *print_n* *print_max*)
                            (dotimes (x (length *current*))
                                (princ (elt *current* x))
                                (cond  ;  Print * after each elt of product, unless is the last one.
                                    ((= x (- (length *current*) 1))
                                        (terpri)
                                    )
                                    (t
                                        (princ '*)
                                    )
                                )
                            )
                        )

                        ((= *print_n* *print_max*)
                            (format t "~%Surpressing print...")
                            (terpri)
                        )
                    )
                    (setf *print_n* (+ *print_n* 1))
                )
                (t
                    (permute_products (elt *sym_list* (+ k 1)) '() (+ k 1))  ;  If we have not reach the last list of input, then we move to permute next list.
                )                
            )
        )
        (t
            (dolist (item unused_syms)
                (permute_products (remove item unused_syms) (cons item partial_perm) k)  ;  Stay keep permuting the current list of input.
            )
        )
    )
)
 
;
; execute it?
;

;(p0)
