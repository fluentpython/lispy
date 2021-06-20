# Changes from Norvig's code

Peter Norvig's [`lis.py`](../norvig/) code is beautiful.

I made a few changes for didactic reasons.
These were the most important changes:

* added type hints;
* in `Procedure.__call__`, changed `env` to `local_env`;
* removed module level `global_env`;
* renamed function `atom` to `parse_atom`;
* renamed function `eval` to `evaluate`;
* in `evaluate`, made `env` parameter mandatory.

I also rewrote Norvig's custom `lispytest.py` script as
`lis_test.py`, to run with [pytest](https://docs.pytest.org).
My rewrite includes Norvig's original 
[`lis_tests` suite](https://github.com/norvig/pytudes/blob/60168bce8cdfacf57c92a5b2979f0b2e95367753/py/lispytest.py#L5)
adds a few more test cases.

_LR_<br/>
São Paulo, June 19, 2021
