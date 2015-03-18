(io:format "Loading Chapter 1 code ...~n")

;; Expressions

42
(- 309 267)
(+ 1000 337)
(* 5 99)
(/ 10 5)
(+ 2.7 10)
(+ 21 35 12 7)
(* 25 4 12)
(+ (* 3 5) (- 10 6))
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;; Naming and the Environment

(set size 2)
size
(* 5 size)
(set pi 3.14159)
(set radius 10)
(* pi (* radius radius))
(set circumference (* 2 pi radius))
circumference
(set identity (lambda (x) x))
(funcall identity 2)
(defun identity (x) x)
(identity 2)

;; Evaluating Combinations

(* (+ 2 (* 4 6))
   (+ 3 5 7))
(defun square (x) (* x x))
(square 21)
(square (+ 2 5))
(square (square 3))
(defun sum-of-squares (x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
(defun f (a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)

;; The Substitution Model for Function Application

(f 5)
(sum-of-squares (+ 5 1) (* 5 2))
(+ (square 6) (square 10))
(+ (* 6 6) (* 10 10))
(+ 36 100)

;; Conditional Expressions and Predicates

(defun abs (x)
  (cond ((> x 0) x)
        ((== x 0) 0)
        ((< x 0) (- x))))
(defun abs (x)
  (cond ((< x 0) (- x))
        ('true x)))
(defun abs (x)
  (if (< x 0)
      (- x)
      x))
(defun abs (x)
  (case (< x 0)
        ('true (- x))
        (_ x)))
(defun abs
  ((x) (when (> x 0)) x)
  ((x) (when (== x 0)) 0)
  ((x) (when (< x 0)) (- x)))
(defun abs
  ((x) (when (< x 0)) (- x))
  ((x) x))
(defun gte (x y)
  (or (> x y) (== x y)))
(defun gte (x y)
  (not (< x y)))

;; Example: Square Roots by Newton's Method

(defun sqrt (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x)
            x)))
(defun improve (guess x)
  (average guess (/ x guess)))
(defun average (x y)
  (/ (+ x y) 2))
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
(defun sqrt (x)
  (sqrt (* 0.5 x) x))
(sqrt 1)
(sqrt 2)
(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

;; Functions as Black-Box Abstractions

(defun square (x) (* x x))
(defun square (x)
  (exp (double (log x))))
(defun double (x) (+ x x))
(defun square (x) (* x x))
(defun square (y) (* y y))
(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
(defun sqrt (x)
  (sqrt (* 0.5 x) x))

(defun sqrt (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x)
            x)))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun abs
  ((x) (when (< x 0)) (- x))
  ((x) x))

(defun square (x) (* x x))

(defun sqrt (x)
  (sqrt (* 0.5 x) x))

(defun sqrt (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt (improve guess x)
            x)))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun sqrt (x)
  (fletrec ((improve (guess x)
              ;; Improve a given guess for the square root.
              (average guess (/ x guess)))
            (good-enough? (guess x)
              ;; A predicate which determines if a guess is
              ;; close enough to a correct result.
              (< (abs (- (square guess) x)) 0.001))
            (sqrt (guess x)
              ;; A recursive function for approximating
              ;; the square root of a given number.
              (if (good-enough? guess x)
                  guess
                  (sqrt (improve guess x)
                        x))))
    ;; the main body of the function
    (sqrt (* 0.5 x) x)))

(defun sqrt (x)
  (fletrec ((improve (guess)
              ;; Improve a given guess for the square root.
              (average guess (/ x guess)))
            (good-enough? (guess)
              ;; A predicate which determines if a guess is
              ;; close enough to a correct result.
              (< (abs (- (square guess) x)) 0.001))
            (sqrt-rec (guess)
              ;; A recursive function for approximating
              ;; the square root of a given number.
              (if (good-enough? guess)
                  guess
                  (sqrt-rec (improve guess)))))
    ;; the main body of the function
    (sqrt-rec (* 0.5 x))))

(c "ch1/sqrt.lfe")
(sqrt:sqrt 25)
(sqrt:sqrt 1)
(sqrt:sqrt 2)
;; This next one should throw an error
;; (sqrt:sqrt 1 25)

;; Linear Recursion and Iteration

(defun factorial (n)
  (if (== n 1)
      1
      (* n (factorial (- n 1)))))

(defun factorial (n)
  (factorial 1 1 n))

(defun factorial (product counter max-count)
  (if (> counter max-count)
      product
      (factorial (* counter product)
                 (+ counter 1)
                 max-count)))

(defun fib
  ((0) 0)
  ((1) 1)
  ((n)
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defun fib
  ((n) (when (>= n 0))
    (fib n 0 1)))

(defun fib
  ((0 result _)
    result)
  ((n result next)
    (fib (- n 1) next (+ result next))))

(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((== amount 0) 1)
        ((or (< amount 0) (== kinds-of-coins 0)) 0)
        ('true (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
  (cond ((== kinds-of-coins 1) 1)
        ((== kinds-of-coins 2) 5)
        ((== kinds-of-coins 3) 10)
        ((== kinds-of-coins 4) 25)
        ((== kinds-of-coins 5) 50)))

(count-change 100)

;; Exponentiation

(defun expt (b n)
  (if (== n 0)
      1
      (* b (expt b (- n 1)))))

(defun expt (b n)
  (expt b n 1))

(defun expt (b counter product)
  (if (== counter 0)
      product
      (expt b
            (- counter 1)
            (* b product))))

(defun fast-expt (b n)
  (cond ((== n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        ('true (* b (fast-expt b (- n 1))))))

(defun even? (n)
  (=:= 0 (rem (trunc n) 2)))

;; Greatest Common Divisors

(defun gcd (a b)
  (if (== b 0)
      a
      (gcd b (rem a b))))

;; Example: Testing for Primality

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ('true (find-divisor n (+ test-divisor 1)))))

(defun divides? (a b)
  (== (rem b a) 0))

(defun prime? (n)
  (== n (smallest-divisor n)))

(defun expmod (base exp m)
  (cond ((== exp 0) 1)
        ((even? exp)
         (rem (square (expmod base (/ exp 2) m))
              m))
        ('true
         (rem (* base (expmod base (- exp 1) m))
              m))))

(defun fermat-test (n)
  (flet ((try-it (a)
           (== (expmod a n n) a)))
    (try-it (random:uniform (- n 1)))))

(defun fast-prime? (n times)
  (cond ((== n 1) 'true)
        ((== times 0) 'true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        ('true 'false)))

;; Formulating Abstractions with Higher-Order Functions

(defun cube (x) (* x x x))

;; Functions as Arguments

(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defun sum-cubes (a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun inc (n) (+ n 1))

(defun sum-cubes (a b)
  (sum #'cube/1 a #'inc/1 b))

(sum-cubes 1 10)

(defun identity (x) x)

(defun sum-integers (a b)
  (sum #'identity/1 a #'inc/1 b))

(sum-integers 1 10)

(defun pi-sum (a b)
  (flet ((pi-term (x)
           (/ 1.0 (* x (+ x 2))))
         (pi-next (x)
           (+ x 4)))
    (sum #'pi-term/1 a #'pi-next/1 b)))

(* 8 (pi-sum 1 100000))

(defun integral (f a b dx)
  (flet ((add-dx (x)
           (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx/1 b)
       dx)))

(integral #'cube/1 0 1 0.01)
(integral #'cube/1 0 1 0.001)
(integral #'cube/1 0 1 0.0001)

;; Constructing Functions Using Lambda

(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

(defun pi-sum (a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(defun integral (f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(apply (lambda (x y z) (+ x y (square z))) '(1 2 3))

(defun add-sq (x y z)
  (+ x y (square z)))

(apply #'add-sq/3 '(1 2 3))

(defun f (x y)
  (flet ((f-helper (a b)
          (+ (* x (square a))
             (* y b)
             (* a b))))
    (f-helper (+ 1 (* x y))
              (- 1 y))))

(defun f (x y)
  (funcall
    (lambda (a b)
      (+ (* x (square a))
         (* y b)
         (* a b)))
    (+ 1 (* x y))
    (- 1 y)))

(defun f (x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; Functions as General Methods

(defun negative? (x)
  (< x 0))

(defun positive? (x)
  (> x 0))

(defun search (f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (funcall f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(defun close-enough? (x y)
  (< (abs (- x y)) 0.001))

(defun half-interval-method (f a b)
  (let ((a-value (funcall f a))
        (b-value (funcall f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign")))))

(half-interval-method #'math:sin/1 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                       1.0
                       2.0)

(defun fixed-point (f first-guess)
  (fletrec ((close-enough? (v1 v2)
              (< (abs (- v1 v2)) tolerance))
            (try-it (guess)
              (let ((next (funcall f guess)))
                (if (close-enough? guess next)
                    next
                    (try-it next)))))
    (try-it first-guess)))

(set tolerance 0.00001)

(fixed-point #'math:cos/1 1.0)

(fixed-point (lambda (y) (+ (math:sin y) (math:cos y)))
             1.0)

(defun sqrt (x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(defun sqrt (x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Functions as Returned Values

(defun average-damp (f)
  (lambda (x)
    (average x (funcall f x))))

(funcall (average-damp #'square/1) 10)

(defun sqrt (x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(defun cube-root (x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
       dx)))

(set dx 0.00001)

(defun cube (x) (* x x x))

(funcall (deriv #'cube/1) 5)

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun sqrt (x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(defun sqrt (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))


(io:format "Loaded Chapter 1 code.~n")
