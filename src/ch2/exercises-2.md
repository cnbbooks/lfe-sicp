### Exercises

#### Exercise 2.2

Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor ``make-segment`` and selectors ``start-segment`` and ``end-segment`` that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the \\(x\\) coordinate and the \\(y\\) coordinate. Accordingly, specify a constructor ``make-point`` and selectors ``x-point`` and ``y-point`` that define this representation. Finally, using your selectors and constructors, define a function ``midpoint-segment`` that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your functions, you'll need a way to print points:

 ```lisp
(defun print-point (p)
  (io:format "(~p, ~p)~n" (list (x-point p)
                                (y-point p))))
 ```

#### Exercise 2.3

Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create functions that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area functions will work using either representation?
