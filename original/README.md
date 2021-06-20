# Norvig's originals and updates

This directory contains:

* `norvig/`: Norvig's `lis.py` unchanged, and `lispy.py` with
[minor changes](https://github.com/norvig/pytudes/pull/106) to run on Python 3,
and the `lispytest.py` custom test script;
* `py3.9/`: `lis.py` with type hints and a few minor edits—requires Python 3.9;
* `py3.10/`: `lis.py` with type hints, pattern matching, and minor edits—requires Python 3.10.

The `py3.9/` and `py3.10/` directories also have `lis_test.py` to run with
[pytest](https://docs.pytest.org).
These files include all the
[`lis_tests` suite](https://github.com/norvig/pytudes/blob/60168bce8cdfacf57c92a5b2979f0b2e95367753/py/lispytest.py#L5)
from `original/lispytest.py`,
and additional separate tests for each expression and special form handled by `evaluate`.
