;The Z system <--- Z???
;testtree.l
;Let's set up a function tree and evaluate it

;desired results
(setq f-g (list '(1 2) '(2 3) '(3 4) '(4 5) '(5 6) '(6 7) '(7 8)))

;the function we want
(defun want-func(x)
  (+ x 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of general math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten (x)
  "descend into the supplied list until an atom is hit.
append the atom to the flattened rest"
  (if (endp x)
      x
    (if (atom (car x ))
	(append (list (car x)) (flatten (cdr x)))
      (append (flatten (car x)) (flatten (cdr x ))))))

;squares x
(defun sqr (x) (* x x))

;Sum the entire list
(defun sum-list(l)
  (reduce '+ l))

;(a + b) / 2
(defun average (a b) ( / (+ a b) 2))

;;;gets the average of a list
(defun average-list (l) 
  (let ((newlist (flatten l)))
    (/ (sum-list newlist)
       (list-length l))))

;;;component-wise addition I think...
;;TODO: Rewrite as one of the map* family
(defun addlists (x y)
 (setq retval nil)
  (loop 	
   (setq retval 
	 (append (list (+ (car x) (car y))) retval))
     (setf x (cdr x))
     (setf y (cdr y))	
     (when (endp  x) (return)))
  retval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of list routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;exchanges the n'th element of a and b
(defun exchange(n a b)
  (let ((temp (nth n b)))
    (setf (nth n b) (nth n a))
    (setf (nth n a) temp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of fuzzy routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fuzzy closeness is needed. [0, 1] 
;TODO: Check if this can be Reduced.
(defun fuzzy-equal (a b)
  (cond 
    ((and (numberp a) (numberp b))	;If both are numbers
     (fuzzy-close a b))
    ((and (listp a) (listp b))			;If both are lists
     (fuzzy-equal-lists a b))
    (t nil)))								;Otherwise nil out.

;Are they identical?
;1 if they are, 0 if they differ by infinity.
;i(a,b) = 1 /( | |a| - |b| | + 1)
(defun fuzzy-close (a b) 
  (if (equal (abs a) (abs b)) 1
	   (/ 1 ( + (abs (- (abs a) (abs b))) 1.0))))

;degree of equality of the two trees
;Will recurse.  ;;TODO: But doesn't!!! dun dun dun
(defun fuzzy-equal-trees (a b)
  (if (or 
       (not (equal (length a) (length b)))	;Length must be equal 
       (not (and (listp a) (listp b)))) 		;Both must be lists
      nil													;fails if condition is not met
      (progn 

	(average-list (mapcar 'fuzzy-equal a  b)))))

		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of random routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;where n is the number of elements, m is the range
;;TODO: WHO AM I?
(defun generate-rand-list (n m)
    (setq temp nil)
    (loop      
     (setq temp (append (list (random m (make-random-state t))) temp))
     (setq n (- n 1))
     (when (= n 0) (return)))
    temp)

(defun getrand (max)
  "Returns between 0 and max."
	(random max (make-random-state t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The actual GP routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;these are our operations
;binary operations
(setq bin-op-list '(+ - * /))
;unary operations
(setq un-op-list '(1+ sin cos log exp abs sqr sqrt))

;sets up a list with the n'th binary operation and params 
(defun bin-term-create (n p1 p2)
  (list (nth n bin-op-list) p1 p2))

;Like bin-term-create, but with unary operations
(defun un-term-create (n p1)
  (list (nth n un-op-list) p1))

;creates a random operation with unevaluated params p1, and p2
(defun rand-tree-node ()
  (if (> (getrand 10) 3)
      (bin-term-create (getrand (length bin-op-list)) 'p1 'p2)
    (un-term-create (getrand (length un-op-list)) 'p1)))

;Sets the left leaf of parent to 'leaf
(defun set-left-leaf (parent leaf)
  (setf (nth 1 parent) leaf))

;Sets the right leaf of parent to 'leaf
(defun set-right-leaf (parent leaf)
  (setf (nth 2 parent) leaf))


(defun shake-p ()
  "Shakes according to a specific predicate"
  (> (getrand 10) 7))

;uses x as an independant variable
;Adds depth d to node node. Recursive
(defun make-tree (node depth)
  (if (eq (length node) 3) ;is node a binary node
      ;;Can we go deeper?
      (if (eq depth 0)
	  ;;this case is simple. Terminals only
	  (progn (setf (nth 1 node) 'x)      
		 (setf (nth 2 node) 'x))

	  ;;This case is the interesting one
	  (progn
	    ;;The left node
	    (if (shake-p) 
		(setf (nth 1 node) 'x)

		(setf 
		 (nth 1 node) 
		 (make-tree 
		  (rand-tree-node) 
		  (- depth 1))))

	  ;the right node
	    (if (shake-p) ;;seventy percent
		(setf  (nth 2 node) 'x)
		(setf (nth 2 node) (make-tree (rand-tree-node) (- depth 1))))))
      

    ;the unary case
  (progn 
      ;Can we go deeper?
    (if (eq depth 0)
	;;this case is simple. Terminals only
	(setf (nth 1 node) 'x)
	;;This case is the interesting one
	(progn
	  ;;The center node
	  (if (shake-p) ;;seventy percent.
	      (setf (nth 1 node) 'x)
	      (setf (nth 1 node) (make-tree (rand-tree-node) (- depth 1))))))))
					;recurse down
  
  node);end of function- returns node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The interface for Lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-tree ( x )
  (make-tree (rand-tree-node) x))

;evaluates the tree
(defun eval-tree (tree)
  (eval (eval tree)))


(defun check-similarity ()
  (let 
      ((i 1) 
       (dummy-tree (new-tree 4))
       test-value)    
  (loop 
   (setq x i)
   (setq test-value (eval-tree dummy-tree))
   (format t "~a\n" test-value)
   (setq i (+ i 1))
   (when (eq i 100) (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; More code


(defmacro generate-lambda-tree(n)
  "Generates a tree and returns the lambda of its function"
  `(lambda (X)
       ,(new-tree n)))

;;Turns a tree into a lambda
(defmacro tree-to-lambda(tree)
  "Takes a tree and turns it into the lambda of its function"
  (eval ``(lambda (X) 
     ,,tree)))

(setq mytree (new-tree 2))

;(funcall (tree-to-lambda mytree) 4)

;(tree-to-lambda mytree)
;mytree


(defun range (s e)
  (unless (= s e)
      (cons s  (range (+ 1 s) e))))

(mapcar (tree-to-lambda mytree) (range 1 10))


