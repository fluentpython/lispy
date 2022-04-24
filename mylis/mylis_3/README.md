Rewriting from scratch, refactoring modules.
`mylis.py` (TBD) will be the main module.

## Changes

Changed behavior since `mylis_2` includes:

### Alternative braces allowed

The characters `([{}])` may be used to build s-expressions.
The close brace must match open brace.

Use them as you wish, but the conventional usage in `mylis_3` is:

- `()` for function application
- `{}` for special forms
- `[]` for everything else

Example 1:

```
{lambda [a b] {if (>= a b) a b}}
```

Example 2:

```
{cond
    [(> x 0) x]
    [(= x 0) 0]
    [(< x 0) (- 0 x)]}
```
