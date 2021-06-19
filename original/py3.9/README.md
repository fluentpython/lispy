# Changes from Norvig's code

Peter Norvig's `lis.py` code is beautiful.

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
My rewrite includes all Norvig's original test cases and
adds a few more. 

_LR_

São Paulo, June 19, 2021
