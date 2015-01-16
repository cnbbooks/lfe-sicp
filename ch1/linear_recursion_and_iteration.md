### Linear Recursion and Iteration

<a name="figure-3"></a>
![A linear recursive process for computing 6 factorial](images/ch1-Z-G-10.png)

**Figure 1.3**:  A linear recursive process for computing $$6!$$.

We begin by considering the factorial function, defined by

$$
\begin{align}
n!=n\ (n-1)\ (n-2)\cdots3\cdot2\cdot1
\end{align}
$$

There are many ways to compute factorials. One way is to make use of the observation that $$n!$$ is equal to $$n$$ times $$(n - 1)!$$ for any positive integer $$n$$:

$$
\begin{align}
n!=\left[n\ (n-1)\ (n-2)\cdots3\cdot2\cdot1\right] = n\ (n-1)!
\end{align}
$$

Thus, we can compute $$n!$$ by computing $$(n - 1)!$$ and multiplying the result by $$n$$. If we add the stipulation that $$1!$$ is equal to $$1$$, this observation translates directly into a function:

```lisp

```