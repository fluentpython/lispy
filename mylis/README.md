# Mylis:

`mylis.py` is a variation of the Python 3.10
[version](../original/py3.10/) of `lis.py`,
adding some features for demonstration.

## Interactive use

Running `mylis.py` without arguments opens a very limited REPL.

At this time the REPL can only handle one complete expression at a time,
so it's more like a calculator—with no error handling.
Plase don't make mistakes ;-)

```
$ ./mylis.py
mylis.py> pi
3.141592653589793
mylis.py> (/ pi 2)
1.5707963267948966
mylis.py> (sin (/ pi 2))
1.0
mylis.py> (define (! n) (if (<= n 1) 1 (* n (! (- n 1)))))
mylis.py> (! 5)
120
mylis.py> (! 42)
1405006117752879898543142606244511569936384000000000
mylis.py>
```

It's better to experiment using source files and
command-line arguments, as presented next.


## Command-line integration

You can run programs written in the supported Scheme subset from the
command-line, like this:

```
$ ./mylis.py examples/newton.scm x=12345654321
111111.0
```

The `x=12345654321` option creates an `x` global variable with the given value.

If you read [examples/newton.scm](examples/newton.scm) you'see that the last line is:

```scheme
(display (sqrt x))
```

The `x` is not defined in the program,
so it must be given as a command-line argument: `x=...`.

Any command-line option with the syntax `<symbol>=<value>`
will be interpreted as a global `(define x value)`
with the limitation that `value` must be an integer, a float or a symbol.

If you forget to provide a required argument,
the interpreter will make a suggestion (but currently it stops at the first issue found):

```
$ ./mylis.py examples/newton.scm
🚨  Undefined 'x'. You can provide it as an option:
    $ mylis.py examples/newton.scm x=<value>
```

_LR_

São Paulo, August 4, 2021
