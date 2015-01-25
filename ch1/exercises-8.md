### Exercises

#### Exercise 1.21

Use the ``smallest-divisor/1`` function to find the smallest divisor of each of the following numbers: 199, 1999, 19999. 

#### Exercise 1.22

Most Lisp implementations include a primitive called ``runtime`` that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds). Erlang has something similar, though a bit more specific: ``timer:tc/1-3`` are functions which execute functions, returning a tuple of elapsed microseconds and result of function call.

The following ``timed-prime-test`` function, when called with an integer ``n`` and times the function it uses to checks if ``n`` is prime. If ``n`` is prime, the procedure prints three asterisks followed by the amount of time used in performing the test. If ``n`` is not prime, it simply prints ``n``.

```lisp
(defun timed-prime-test (n)
  (let ((`#(,elapsed-time ,value) (timer:tc #'prime?/1 `(,n))))
    (report-prime elapsed-time value)))
    
(defun report-prime
  ((elapsed-time 'true)
    (io:format "~p *** ~p~n" `(,n ,elapsed-time)))
  ((elapsed-time 'false)
    (io:format "~p~n" `(,n))))
```

Using these functions, write a funtion ``search-for-primes`` that checks the primality of consecutive odd integers in a specified range. Use your procedure to find the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth of $$\Theta(\sqrt n)$$, you should expect that testing for primes around 10,000 should take about $$\sqrt 10$$ times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the $$\sqrt n$$ prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation? 

#### Exercise 1.23

#### Exercise 1.24

#### Exercise 1.25

#### Exercise 1.26

#### Exercise 1.27

#### Exercise 1.28