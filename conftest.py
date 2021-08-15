import pytest

def pytest_addoption(parser):
    parser.addoption('--slow', action='store_true',
                     help='run tests marked @slow')


def pytest_runtest_setup(item):
    if 'slow' in item.keywords and not item.config.getoption('--slow'):
        pytest.skip('add --slow to run this test')
