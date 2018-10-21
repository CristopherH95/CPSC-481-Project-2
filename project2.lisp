(setq vars '(x y z))
(setq nums '(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9))
(setq ops '(+ - *))

(defun test-expr (vals expr)
    "Test an expression for the given values of X, Y, and Z.
    PARAMETERS:
        vals => list, of the form (X Y Z)
        expr => list, an expression to run with the given values
    RETURNS:
        The number obtained from evaluating the list"
    (progn
        (print expr) 
        (setq x (nth 0 vals))
        (setq y (nth 1 vals))
        (setq z (nth 2 vals))
        (eval expr)))

(defun random-el (seq)
    "Returns a random element from the given sequence using nth and random
    PARAMETERS:
        seq => sequence, accessible by nth, to choose an element from
    RETURNS:
        A single, randomly selected, element from the list"
    (nth (random (length seq)) seq))

(defun gen-expr (&optional (max-len 7) (max-sub 3) (sub-count 0))
    "Generates a random expression of a given maximum element length 
    (counting a sub-list as 1) and a maximum number of sub-lists.
    OPTIONAL PARAMETERS:
        max-len => number, represents max expression length
        max-sub => number, represents max number of sub-lists
        sub-count => number, counter for number of sub-lists
    RETURNS:
        A list which is a random expression which may include:
            X, Y, and/or Z
            the numbers -9 - 9
            the operators +, -, and/or *"
    (let ((expr-len (random max-len))
          (make-sub nil)
          (use-var nil)
          (expr-piece nil)
          (expr '()))
        (progn
            (if (< expr-len 1)
                (setf expr-len (+ expr-len 2))) ; minimum length 2 (to ensure expression is valid)
            (loop while (<= (length expr) expr-len) 
                do (setf make-sub (random 10))
                do (setf use-var (random 10))
                do (cond
                        ((< (length expr) 1) (setf expr-piece (random-el ops))) ; only first elements are operators 
                        ((and (<= make-sub 3) (< sub-count max-sub))    ; 30% chance to insert sub-list
                            (setf expr-piece (gen-expr max-len max-sub (+ sub-count 1))))
                        ((<= use-var 5)     ; 50% chance to insert a variable
                            (setf expr-piece (random-el vars)))
                        (T                  ; default insert a number
                            (setf expr-piece (random-el nums))))
                collect expr-piece into expr
                finally 
                    (return-from gen-expr expr)))))

(defun validate-gen-expr ()
    "Test function to verify that the function gen-expr is creating valid expressions"
    (loop for i from 1 to 100
        do (print (test-expr (list 1 1 1) (gen-expr)))))