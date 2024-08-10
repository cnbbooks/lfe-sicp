### Exercises

#### Exercise 2.4

Here is an alternative procedural representation of pairs. For this representation, verify that ``(car3 (cons3 x y))`` yields ``x`` for any objects ``x`` and ``y``.

```lisp
(defun cons3 (x y)
  (lambda (m) (funcall m x y)))

(defun car3 (z)
  (funcall z (lambda (p q) p)))
```

What is the corresponding definition of ``cdr3/1``? (Hint: To verify that this works, make use of the substitution model from the section [The Substitution Model for Function Application]().)

#### Exercise 2.5

Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair \\(a\\) and \\(b\\) as the integer that is the product \\(2^a \cdot 3^b\\). Give the corresponding definitions of the functions ``cons4/2``, ``car4/1``, and ``cdr4/1``.

#### Exercise 2.6

In case representing pairs as functions wasn't mind-boggling enough, consider that, in a language that can manipulate functions, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

```lisp
(defun zero
  (lambda (f)
    (lambda (x) x)))

(defun add-1 (n)
  (lambda (f)
    (lambda (x)
      (f (funcall (funcall n f) x)))))
```
This representation is known as *Church numerals*, after its inventor, Alonzo Church, the logician who invented the \\(\lambda\\)-calculus.

Define ``one`` and ``two`` directly (not in terms of ``zero/1`` and ``add-1/1``). (Hint: Use substitution to evaluate ``(funcall add-1 zero)``). Give a direct definition of the addition function ``#'+/2`` (not in terms of repeated application of ``add-1/1``).
