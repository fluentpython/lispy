# Exercises doable with Mylis

## 1. Temperature converter

Write functions `c-to-f` to convert a temperature from Celsius to
Fahrenheit and `f-to-c` to convert back.

Some examples:

* 0°C is 32°F
* 20°C is 68°F
* 100°C is 212°F
* -20°C is -4°F

The formulas are:

* f = (9 / 5) * f + 32
* c = (f − 32) * 5 / 9


## 2. Ackermann function

Implement the Ackermann–Péter function.

The
[Ackermann function](http://en.wikipedia.org/wiki/Ackermann_function)
is a deeply recursive function from computability theory.
Besides its theoretical relevance, it has also been used to benchmark
"a compiler's ability to optimize recursion" according to its [Wikipedia article](http://en.wikipedia.org/wiki/Ackermann_function).

One of its variations, the Ackermann–Péter function,
can be coded in Python as:

```python
def ackermann(m, n):
    if m == 0:
        return n + 1
    if n == 0:
        return ackermann(m - 1, 1)
    return ackermann(m - 1, ackermann(m, n - 1))
```

The arguments `m` and `n` must non-negative integers.

In practice, they must also be very small.
Wikipedia says "A(4, 2) is an integer of 19,729 decimal digits."

Computing A(3, 2) requires 541 calls to `ackermann`,
and the result is 29.

In Python 3.10, the two largest combinations of `(m, n)` that
can be computed with the definition above are:

* A(3, 6) = 509 (requires 172,233 calls)
* A(4, 0) = 13 (requires 107 calls)

Using memoization with `functools.cache`, the largest combinations are:

* A(3, 7) = 1021 (requires 2,558 calls)
* A(4, 0) = 13 (requires 33 calls)

See
[exercises/ackermann.py](https://github.com/fluentpython/lispy/blob/main/mylis/exercises/ackermann.py)
for a Python script that iterates over small values of `(m, n)` and
counts the number of calls required.
