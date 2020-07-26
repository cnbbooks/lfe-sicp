(defmodule sqrt
  (export (sqrt 1)))

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
