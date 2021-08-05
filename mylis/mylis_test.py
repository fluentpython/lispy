import io

from pytest import mark

from mylis import env_from_args, run_file

@mark.parametrize("args, global_env", [
    ([], {}),
    (['x'], {}),
    (['a=2'], {'a': 2}),
    (['=', 'a=.5', 'flavor=banana', 'max=999', '=='],
     {'a': 0.5, 'flavor':'banana', 'max': 999}),
])
def test_env_from_args(args, global_env):
    got = env_from_args(args)
    assert got == global_env


def test_run_file(capsys):
    source = """
    (define (! n)
        (if (<= n 1)
            1
            (* n (! (- n 1)))))
    (display (! x))
    """
    file = io.StringIO(source)
    env = env_from_args(['x=5'])
    got = run_file(file, env)
    assert got == '120'
    captured = capsys.readouterr()
    assert captured.out == '120\n'
