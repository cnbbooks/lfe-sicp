### Orders of Growth

The previous examples illustrate that processes can differ considerably in the rates at which they consume computational resources. One convenient way to describe this difference is to use the notion of *order of growth* to obtain a gross measure of the resources required by a process as the inputs become larger.

Let $$n$$ be a parameter that measures the size of the problem, and let $$R(n)$$ be the amount of resources the process requires for a problem of size $$n$$. In our previous examples we took n to be the number for which a given function is to be computed, but there are other possibilities. For instance, if our goal is to compute an approximation to the square root of a number, we might take $$n$$ to be the number of digits accuracy required. For matrix multiplication we might take n to be the number of rows in the matrices. In general there are a number of properties of the problem with respect to which it will be desirable to analyze a given process. Similarly, $$R(n)$$ might measure the number of internal storage registers used, the number of elementary machine operations performed, and so on. In computers that do only a fixed number of operations at a time, the time required will be proportional to the number of elementary machine operations performed.

We say that $$R(n)$$ has order of growth $$\Theta(f(n))$$, written $$R(n) = \Theta(f(n))$$ (pronounced "theta of $$f(n)$$"), if there are positive constants $$k1$$ and $$k2$$ independent of n such that 

$$
k_1 f(n) \le R(n) \le k_2 f(n)
$$

for any sufficiently large value of $$n$$. (In other words, for large $$n$$, the value $$R(n)$$ is sandwiched between $$k_1f(n)$$ and $$k_2f(n)$$.)

For instance, with the linear recursive process for computing factorial described in the section [Linear Recursion and Iteration]() the number of steps grows proportionally to the input $$n$$. Thus, the steps required for this process grows as $$\Theta(n)$$. We also saw that the space required grows as $$\Theta(n)$$. For the iterative factorial, the number of steps is still $$\Theta(n)$$ but the space is $$\Theta(1)$$ -- that is, constant.[^1] The tree-recursive Fibonacci computation requires $$\Theta(\phi^n)$$ steps and space $$\Theta(n)$$, where $$\phi$$ is the golden ratio described in the section [Tree Recursion]().

Orders of growth provide only a crude description of the behavior of a process. For example, a process requiring $$n^2$$ steps and a process requiring $$1000 n^2$$ steps and a process requiring $$3n^2 + 10n + 17$$ steps all have $$\Theta(n2)$$ order of growth. On the other hand, order of growth provides a useful indication of how we may expect the behavior of the process to change as we change the size of the problem. For a $$\Theta(n)$$ (linear) process, doubling the size will roughly double the amount of resources used. For an exponential process, each increment in problem size will multiply the resource utilization by a constant factor. In the remainder of the section [Procedures and the Processes They Generate]() we will examine two algorithms whose order of growth is logarithmic, so that doubling the problem size increases the resource requirement by a constant amount.

----

[^1]: These statements mask a great deal of oversimplification. For instance, if we count process steps as "machine operations" we are making the assumption that the number of machine operations needed to perform, say, a multiplication is independent of the size of the numbers to be multiplied, which is false if the numbers are sufficiently large. Similar remarks hold for the estimates of space. Like the design and description of a process, the analysis of a process can be carried out at various levels of abstraction. 





