;; Load the code from Chapter 1

(run "ch1/code.lfe")
(io:format "Loading Chapter 2 code ...~n")

;; Building Abstractions with Data

(defun linear-combination (a b x y)
  (+ (* a x) (* b y)))

(defun linear-combination (a b x y)
  (add (mul a x) (mul b y)))

;; Example: Arithmetic Operations for Rational Numbers

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defun equal-rat? (x y)
  (== (* (numer x) (denom y))
      (* (numer y) (denom x))))

(set x (cons 1 2))
(car x)
(cdr x)
(set x (cons 1 2))
(set y (cons 3 4))
(set z (cons x y))
(car (car z))
(car (cdr z))

(defun make-rat (x y)
  (cons x y))

(defun numer (rat)
  (car rat))

(defun denom (rat)
  (cdr rat))

(defun numer
  (((cons x _))
    x))

(defun denom
  (((cons _ y))
    y))

(defun print-rat (rat)
  (io:format "~p/~p~n" (list (numer rat)
                             (denom rat))))

(set one-half (make-rat 1 2))
(print-rat one-half)
(set one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cons (trunc (/ n g))
          (trunc (/ d g)))))

(print-rat (add-rat one-third one-third))

;; Abstraction Barriers

(defun make-rat (n d)
  (cons n d))

(defun numer (x)
  (let ((g (gcd (car x) (cdr x))))
    (trunc (/ (car x) g))))

(defun denom (x)
  (let ((g (gcd (car x) (cdr x))))
    (trunc (/ (cdr x) g))))

(defun cons2 (x y)
  (match-lambda
    ((0) x)
    ((1) y)
    ((n) (error (++ "cons2: argument must be 0 or 1 -- got "
                    (integer_to_list n))
                'bad-argument))))

(defun car2 (z)
  (funcall z 0))

(defun cdr2 (z)
  (funcall z 1))

;; Extended Exercise: Interval Arithmetic

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Representing Sequences

(cons 1 (cons 2 (cons 3 (cons 4 '()))))
(set one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cons 10 one-through-four)
(cons 5 one-through-four)

;; List Operations

(defun list-ref
  ((items 1)
   (car items))
  ((items n)
   (list-ref (cdr items) (- n 1))))

(set squares (list 1 4 9 16 25))
(list-ref squares 4)

(defun list-ref
  (((cons head _) 1)
   head)
  (((cons _ tail) n)
   (list-ref tail (- n 1))))

(defun len
  (('())
   0)
  ((items)
   (+ 1 (len (cdr items)))))

(set odds (list 1 3 5 7))
(len odds)

(defun len
  (('())
   0)
  (((cons _ tail))
   (+ 1 (len tail))))

(defun len (items)
  (len items 0))

(defun len
  (('() count)
   count)
  (((cons _ tail) count)
   (len tail (+ 1 count))))

(defun append
  (('() list2)
   list2)
  ((list1 list2)
   (cons (car list1) (append (cdr list1) list2))))

(append squares odds)

(defun append
  (('() list2)
   list2)
  (((cons head tail) list2)
   (cons head (append tail list2))))

(append odds squares)

;; Mapping over lists

(defun scale-list
  (('() _)
     '())
       (((cons head tail) factor)
          (cons (* head factor)
                   (scale-list tail factor))))

(scale-list (list 1 2 3 4 5) 10)

(defun mapper
  ((_ '())
     '())
       ((func (cons head tail))
          (cons (funcall func head)
                   (mapper func tail))))

(mapper #'abs/1 (list -10 2.5 -11.6 17))
(mapper (lambda (x) (* x x))
   (list 1 2 3 4))

(defun scale-list (items factor)
  (mapper
      (lambda (x) (* x factor))
          items))

;; Hierarchical Structures

(cons (list 1 2) (list 3 4))
(set x (cons (list 1 2) (list 3 4)))
(len x)

(defun count-leaves
  (('()) 0)
  (((cons head tail))
    (+ (count-leaves head)
       (count-leaves tail)))
  ((_) 1))

(count-leaves x)
(list x x)
(len (list x x))
(count-leaves (list x x))

;; Mapping over trees

(defun scale-tree
  (('() _)
   '())
  (((cons head tail) factor)
   (cons (scale-tree head factor)
         (scale-tree tail factor)))
  ((tree factor)
    (* tree factor)))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 100)

(defun scale-tree (tree factor)
  (lists:map #'scale-sub-tree/1 tree))

(defun scale-sub-tree
  ((sub-tree) (when (is_integer sub-tree))
   (* sub-tree factor))
  ((sub-tree)
   (scale-tree sub-tree factor)))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 100)

(defun odd? (n)
  (=:= 1 (rem (trunc n) 2)))

(defun sum-odd-squares
  (('())
    0)
  (((cons head tail))
    (+ (sum-odd-squares head)
       (sum-odd-squares tail)))
  ((elem)
    (if (odd?)
        (square elem)
        0)))

(defun even-fibs (n)
  (fletrec ((next (k)
              (if (> k n)
                  '()
                  (let ((f (fib k)))
                    (if (even? f)
                        (cons f (next (+ k 1)))
                        (next (+ k 1)))))))
    (next 0)))

;; Sequence Operations

(mapper #'square/1 (list 1 2 3 4 5))

(defun filter
 ((_ '())
  '())
 ((predicate (cons head tail))
  (if (funcall predicate head)
      (cons head (filter predicate tail))
      (filter predicate tail))))

(filter #'odd?/1 (list 1 2 3 4 5))

(io:format "Chapter 2 loaded.~n")
