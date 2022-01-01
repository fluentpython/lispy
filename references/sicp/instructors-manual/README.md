# Concrete Metacircular Evaluator

Code adapted from the "evaluator with concrete syntax" listed on pages 121-122
of the "Instructor's Manual to accompany Structure and Interpretation of
Computer Programs" by Julie Sussman with Harold Abelson and Gerald Jay Sussman.

Changes:

* function names changed to ALL-CAPS to avoid confusion with
  functions provided by Racket;
* parameters named `env` renamed to `environ` to avoid confusion with `exp`;
* use of `#t`, `#f` instead of `true`, `false`;
* APPLY rewritten to avoid the need for `primitive-procedure?`;
  the corresponding function in Racket is `primitive?` but it is not
  useful to us because `(primitive? +)` returns `#f`, so we can't use it
  to distinguish user-defined procedures from the built-ins.

Tested in Racket 8.3.

