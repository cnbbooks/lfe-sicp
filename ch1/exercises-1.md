### Exercises

#### Exercise 1.1

Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.

```lisp
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
```

#### Exercise 1.2

Translate the following expression into prefix form 

$$
\begin{align}
\frac{5+4+(2-(3-\left(6+\frac{4}{5}\right)}
{3(6-2)(2-7)}
\end{align}
$$

#### Exercise 1.3

Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers. 

#### Exercise 1.4

Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure: 

```lisp
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0)
               #'+/2
               #'-/2)
           a b)))
```

#### Exercise 1.5

Alice Algol has invented a test to determine whether the interpreter she is faced with is using applicative-order evaluation or normal-order evaluation. She defines the following two procedures:

```lisp
(defun p () (p))
```

```lisp
(defun test (x y)
  (if (== x 0)
      0
      y))
```

Then she evaluates the expression

```lisp
(test 0 (p))
```

What behavior will Alice observe with an interpreter that uses applicative-order evaluation? What behavior will she observe with an interpreter that uses normal-order evaluation? Explain your answer. (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.) 


