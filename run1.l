;functions
(setq func-list (list
		 (lambda (a b) (+ a b))
		 (defun f1 (a b) (- a b))
		 (defun f2 (a b) (* a b))))

;desired results
(setq f-g (list '(0 1) '(1 2) '(2 3) '(3 4) '(4 5) '(5 6) '(6 7) '(7 8)))

(defun sqr (x) (* x x))

;1 /( |a| - |b| + 1)
(defun fuzzyclose (a b) 
  (if (equal (abs a) (abs b)) 1
	   (/ 1 ( + (abs (- (abs a) (abs b))) 1.0))))

;fuzzy closeness is needed. [0, 1] 
(defun fuzzyequal (a b)
   (if (and (numberp a) (numberp b))
       (fuzzyclose a b)
     nil))

(defun sum-list (l)
  (if (not (endp l))
      (+ (car l) (sum-list (cdr l)))
    0))

;(a + b) / 2
(defun average (a b) ( / (+ a b) 2))

;gets the average of a list
(defun average-list (l) 
	(if (not (endp l)) (/ (sum-list l) (list-length l))))

;component-wise addition i think...
(defun addlists (x y)
  (setq retval nil)
  (loop 
   (setq retval (append (list (+ (car x) (car y))) retval))
   (setf x (cdr x))
   (setf y (cdr y))	
   (when (endp  x) (return)))
  retval)

;applies f to the two lists, returns a list.
(defun applyfunc (f a b)
  (let ((retval nil)
	(newa a)
	(newb b))
    (loop 
     (setq retval (append (list (funcall f (car newa) (car newb))) retval))
     (setq newa (cdr newa))
     (setq newb (cdr newb))
     (when (endp newa) (return)))
    (reverse retval)))

;degree of equality of the two lists
;fuzzily equal each member and average that sum.
;Sum all the fuzzy equals, then average.
(defun fuzzyequal-lists (a b)
  	(setq x 0)
	(setq n (length a))
	(average-list (applyfunc 'fuzzyequal a  b)))

;where n is the number of elements, m is the range
(defun fill-list-rand (n m)
    (setq temp nil)
    (loop      
     (setq temp (append (list (random m (make-random-state t))) temp))
     (setq n (- n 1))
     (when (= n 0) (return)))
    temp)


(setq list1 (fill-list-rand 100 5))
(setq list2 (fill-list-rand 100 5))

(write (fuzzyequal-lists list1 list2))
(write list1)
(write list2)
;(defun run-me()
;  (write (applyfunc 'fuzzyequal list2 list1))

