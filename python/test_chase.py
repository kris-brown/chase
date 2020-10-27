# import pytest
from chase import Rel, Inst, Match, And, EGD, TGD
# Val, Var, Const, Tup, , Query,

"""
Run with python -m pytest test_chase.py
"""

i1 = Inst.fromdict(A=(['col1', 'col2', 'col3'],
                      [[1, 'x', 2],
                      ['c', 'd', 3]]),
                   B=(['col2', 'col4'],
                      [['x', 'x'],
                      ['x', 'y']]))

iTriangle = Inst.fromdict_noattr(R=[[1, 2], [2, 3], [3, 1]])
iTriangle11 = Inst.fromdict_noattr(R=[[1, 1], [1, 2], [2, 3], [3, 1]],
                                   N=[['a']])

q1 = Match.fromlist('A', ['c', 4, 5])
q2 = Match.fromlist('B', ['x', 4])
q3 = And(q1, q2)

ab = i1['A'].join(i1['B'])
egd = EGD(Match.fromlist('R', [0, 1]))
tgd = TGD.fromlist(Match.fromlist('R', [0, 0]), N=[1])


def test_validate_match() -> None:
    """Check whether ..."""
    res = Rel.fromlist('', [['x', 2], ['d', 3]], ['4', '5'])
    assert q1.run(i1) == res
    res = Rel.fromlist('', [['x'], ['y']], ['4'])
    assert q2.run(i1) == res


if __name__ == '__main__':
    print('q1\n', q1.run(i1))
    print('q2\n', q2.run(i1))
    print('q3\n', q3.run(i1))
    print('\n\nab\n', ab)
    print('\n\negd.query.match\n', egd.q.run(iTriangle))
    print('\n\negd.query.match\n', egd.fire(iTriangle))
    print('\n\negd.query.match\n', tgd.fire(iTriangle11))
