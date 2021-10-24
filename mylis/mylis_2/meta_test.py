import operator as op
from pathlib import Path

from pytest import mark

import mylis
import lis

META_SCM = Path('meta.scm').read_text()


def test_ENV_data():
    source = META_SCM + 'ENV'
    got = mylis.run(source)
    want = (['NOT', op.not_], ['EQ?', op.is_], ['EQUAL?', op.eq])
    assert all(pair in got for pair in want)


def test_LOOKUP():
    source = META_SCM + '(LOOKUP (quote EQ?) ENV)'
    got = mylis.run(source)
    assert op.is_ == got


def test_LOOKUP_is_case_sensitive():
    source = META_SCM + '(LOOKUP (quote eq?) ENV)'
    got = mylis.run(source)
    assert False is got


def test_EVAL_apply():
    exp = '(ADD 1 2 3 4)'
    apply_src = f'(apply (LOOKUP (FIRST (quote {exp})) ENV) (REST (quote {exp})))'
    source = META_SCM + apply_src
    got = mylis.run(source)
    assert 10 is got


@mark.parametrize('exp, want', [
    ('1.5', 1.5),
    ('EQUAL?', op.eq),
    ('(QUOTE Ni!)', 'Ni!'),
    ('(IF 1 10 20)', 10),
    ('(IF 0 10 20)', 20),
    ('(IF (EQUAL? 3 3) (QUOTE #T) (QUOTE #F))', '#T'),
    ('(IF (EQUAL? 3 4) (QUOTE #T) (QUOTE #F))', '#F'),
    ('(MUL (DIV 300 400) 100)', 75),
])
def test_EVAL(exp, want):
    lis.TCO_ENABLED = False
    source = META_SCM + f'(EVAL (quote {exp}) ENV)'
    got = mylis.run(source)
    assert want == got
    lis.TCO_ENABLED = True


@mark.parametrize('exp, want', [
    ('(1 2 3)', [1, 2, 3]),
    ('((ADD 1 2) (ADD 3 4))', [3, 7]),
])
def test_EVLIS(exp, want):
    lis.TCO_ENABLED = False
    source = META_SCM + f'(EVLIS (quote {exp}) ENV)'
    got = mylis.run(source)
    assert want == got
    lis.TCO_ENABLED = True
