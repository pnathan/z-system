;;;;Genetic programming program
;;;;Let's set up a function tree and evaluate it


(defun range (start end)
  (assert (<= start end))
  (loop for n from start below end
        collect n))

;the function we want
(defun want-func (x)
  "Goal function"
  (+ x 1))

(defvar *test-input-output* (mapcar #'(lambda (x) 
					(list x (want-func x))) 
				    (range 2 100))
  "The desired results")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;A set of general math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun range (s e)
;;   (unless (= s e)
;;       (cons s  (range (+ 1 s) e))))


(defun flatten (tree)
  "descend into the supplied list until an atom is hit.
append the atom to the flattened rest"
  (if (endp tree)
      x
    (if (atom (car tree ))
	(append (list (car tree)) (flatten (cdr tree)))
      (append (flatten (car tree)) (flatten (cdr tree ))))))

(defun sqr (x) 
  "squares x"
  (* x x))

(defun sum-list(l)
  "Sum the entire list"
  (reduce '+ l))

;(a + b) / 2
(defun average (a b) 
  ( / (+ a b) 2))


(defun average-list (l) 
  "gets the average of a list"
  (let ((newlist (flatten l)))
    (/ (sum-list newlist)
       (list-length l))))

;;;component-wise addition I think...
;;TODO: Rewrite as one of the map* family
;;mapcar #'+ x y
(defun addlists (x y)
  (mapcar #'+ x y))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of list routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;exchanges the n'th element of a and b
;; Returns the modified a,b in a list
;; (defun exchange(n a b)
;; "Creates new values and swaps."
;;   (let ((temp (nth n b))
;; 	(newa a)
;; 	(newb b))	
;;     (setf (nth n newb) (nth n a))
;;     (setf (nth n newa) (nth n b))
;;     (list newa newb)
;;     ))


(defmacro swap (a b)
  `(psetf ,a ,b  ;;todo look this up
          ,b ,a))
 
(defun exchange (index list-a list-b)
  "Conses two new lists and swaps their respective elements at index."
  (let ((new-a (copy-list list-a))
        (new-b (copy-list list-b)))
    (swap (nth index new-a)
          (nth index new-b))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of fuzzy routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fuzzy closeness is needed. [0, 1] 
;TODO: Check if this can be Reduced.
(defun fuzzy-equal (a b)
  (cond 
    ((and (numberp a) (numberp b))	;If both are numbers
     (fuzzy-close a b))
    ((and (listp a) (listp b))	        ;If both are lists
     (fuzzy-equal-lists a b))
    (t nil)))				;otherwise nil out.


(defun euclidean (x1 x2)
  "euclidean distance"
  (sqrt 
   (reduce '+ 
	   (map 'list 
		#'(lambda (a b) (sqr (-  a  b)))
		x1
		x2))))

;degree of equality of the two trees
;Will recurse.  ;;TODO: But doesn't!!! dun dun dun
(defun fuzzy-equal-trees (a b)
  (if (or 
       (not (equal (length a) (length b)))	;Length must be equal 
       (not (and (listp a) (listp b)))) 	;Both must be lists
      nil				      	;fails if condition is not met
    (progn       
	(average-list (mapcar 'fuzzy-equal a  b)))))

;;;Dissertating on the trees equality - 
;; Shape equality
;; shape-value equality
;; flattened, ordered, value equality 		
;; flattened-unordered equality

;;honking slow
(defun generic-le (a b)
  (let ((str-a (format nil "~a" a))
	(str-b (format nil "~a" b)))
    ( numberp (STRING< str-a str-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of random routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun generate-random-list (length max)
  (loop for n from 0 below length
        for x = (random m)
        collect x))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The actual GP routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (defun check-similarity ()
;;   (let 
;;       ((i 1) 
;;        (dummy-tree (new-tree 4))
;;        test-value)    
;;   (loop 
;;    (setq x i)
;;    (setq test-value (eval-tree dummy-tree))
;;    (format t "~a\n" test-value)
;;    (setq i (+ i 1))
;;    (when (eq i 100) (return)))))



;these are our operations
;binary operations
(defvar *binary-operator-list* '(+ - * /))
;unary operations
(defvar *unary-operator-list* '(1+ sin cos log exp abs sqr sqrt))
 
(defun create-binary-term (n p1 p2)
  "Creates a list with the n'th binary operation and parameters p1 and p2."
  (list (nth n *binary-operator-list*) p1 p2))
 
(defun create-unary-term (n p1)
  "Creates a list with the n'th unary operation and parameter p1"
  (list (nth n *unary-operator-list*) p1))
 
(defun create-random-unary (var)
  (create-unary-term (random (length *unary-operator-list*)) var))
 
(defun create-random-binary (var1 var2)
  (create-binary-term (random (length *binary-operator-list*)) var1 var2))
 

;creates a random operation with unevaluated params p1, and p2
(defun rand-tree-node ()
  (if (shake-p)
      (create-random-binary 'x 'x)
    (create-random-unary 'x)))

;Sets the left leaf of parent to 'leaf
(defun set-left-leaf (parent leaf)
  (setf (nth 1 parent) leaf))

;Sets the right leaf of parent to 'leaf
(defun set-right-leaf (parent leaf)
  (setf (nth 2 parent) leaf))


(defparameter *expand-probability* 0.7)
 
(defun shake-p ()
  "General dice roll for determining whether something should be
expanded."
  (> (random 1.0) *expand-probability*))
;;;uses x as an independant variable
;;Adds depth d to node node. Recursive

(defun make-tree (node depth)
  (if (eq (length node) 3) ;Is it of the form (op arg1 arg2)
      ;;Can we go deeper?
      (if (eq depth 0)
          ;;this case is simple. Terminals only
          (setf (nth 1 node) 'x
                (nth 2 node) 'x)
          ;;This case is the interesting one
	(mapcar (lambda (leaf-setter)
		  (funcall leaf-setter node
			   (if (shake-p)
			       'x
			     (make-tree (rand-tree-node) (- depth 1)))))
		'(set-left-leaf  set-right-leaf)))
      ;;Is it of the form (op arg1)
      ;;Can we go deeper?
      (if (eq depth 0)
          ;;this case is simple. Terminals only
          (set-left-leaf node 'x)
          ;;This case is the interesting one
          ;;The center node
          (set-left-leaf node
                         (if (shake-p)
                             'x
                             (make-tree (rand-tree-node) (- depth 1))))))
  node);end of function- returns node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The interface for Lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-tree ( x )
  (make-tree (rand-tree-node) x))

(defmacro generate-lambda-tree(n)
  "Generates a tree and returns the lambda of its function"
  `(lambda (X)
       ,(new-tree n)))

;;Turns a tree into a lambda
(defmacro tree-to-lambda(tree)
  "Takes a tree and turns it into the lambda of its function"
  (eval ``(lambda (X) 
     ,,tree)))

(defun make-trees (n)
  "Makes n trees"
  (if (> n 0) 
      (cons (new-tree 2) (make-trees (- n 1)))))

(defmacro make-functions-macro (tree)
  `(if ,tree
      (cons 
       (tree-to-lambda (car ,tree))
       (make-functions-macro (cdr ,tree)))))

(defun eval-trees (tau-list inputs)
  (if tau-list
      (cons
       (mapcar (car tau-list) inputs)
       (eval-trees (cdr tau-list) inputs))))

(defun rotate (&rest list-of-lists)
  "Suppose each list was a row in a matrix. Then rotate swaps the rows
and columns"
  (apply #'mapcar #'list list-of-lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-tree-lambda (depth)
  (list 'lambda '(x)
        (new-tree depth)))

(defun make-tree-function (lambda-tree)
  (eval lambda-tree))

(defun eval-fitness (lambda-form-list input-output-list)
  "Determines how well the lambda forms approach the wanted function
by comparing their output with the wanted output in the supplied test
cases.  Returns a list of mean quadratic error sums."
  (mapcar (lambda (lambda-form)
            (let* ((actual-results (mapcar (make-tree-function lambda-form)
                                           (mapcar #'first input-output-list)))
                   (differences (mapcar #'-
                                        actual-results
                                        (mapcar #'second input-output-list)))
                   (squared-differences (mapcar #'square
                                                differences)))
              (/ (reduce #'+ squared-differences)
                 (length squared-differences))))
          lambda-form-list))

(defun tree-fitness (tree-list input-output-list)
  "Creates a list of lists, each inner list is (tree fitness). Input
is a list of trees, and a list of test cases."
  (mapcar (lambda (tree fitness)
            (list tree fitness))
          tree-list
          (eval-fitness (mapcar #'make-tree-lambda tree-list)
                        input-output-list)))

(defvar tree (make-tree-lambda 10))

(setq tree (make-tree-lambda 4))

(funcall (make-tree-function  (make-tree-lambda 4)) 1)

(defvar others '(((1 2 3) (4 5 6))
	       ('a 'b) 
	       (11 12)
	       (8 9)))



(make-tree-function (new-tree 3))



      
;(defvar treelist2 (eval `(make-trees 2)))
;; (setq taus2 (make-functions-macro treelist2))
;; (setq tau-evaled2 (eval-trees taus2 (range 2 10)))
;; (rotate (list treelist2 taus2 tau-evaled2))

;(make-trees 2)
;(macroexpand-1 '(make-trees 2))
;(setq samples (return-sample 2))

;(rotate samples)



;; (setq a-result (mapcar (tree-to-lambda treea) (range 2 10)))
;; (setq b-result (mapcar (tree-to-lambda treeb) (range 2 10)))
;; (setq c-result (mapcar (tree-to-lambda treec) (range 2 10)))

;; (setq want-result (mapcar 'want-func (range 2 10)))

;; (euclidian want-result a-result)
;; (euclidian want-result b-result)
;; (euclidian want-result c-result)
