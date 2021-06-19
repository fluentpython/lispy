# Changes for Python 3.10

This is a variation of the Python 3.9
[version](../py3.9/) of `lis.py`,
to demonstrate some features of Python 3.10.

The most important change is using pattern matching in `evaluate()`.
Language processing is great to show why `match/case`
is much more than a `switch` statement.
To showcase nested list destructuring in `case`,
I added support to the `(define (fun parm...) body)` special form.

Other Python 3.10 features used:

* union types written as `A | B` instead of `Union[A, B]`;
* explicit type alias definitions with `typing.TypeAlias`.


_LR_
São Paulo, June 19, 2021
