# import pytest
from typing import Any, Tuple as T, List as L, TypeVar, Callable as C
from chase import (Var, Tup, Const, Rel, Inst, Match, And, EGD, TGD, Query,
                   Dependency, Atom, Val)
from hypothesis import note, assume, given, strategies as s
import string
"""
Run with python -m pytest test_chase.py
"""


##########################
# Hypothesis strategies #
##########################
Typ = TypeVar("Typ")
Draw = C[[s.SearchStrategy[Typ]], Typ]


def choice(xs: L[Typ]) -> s.SearchStrategy[Typ]:
    '''Randomly pick an element of a fixed list'''
    return s.one_of(*map(s.just, xs))


n_1_3 = s.integers(min_value=1, max_value=3)
n_0_5 = s.integers(min_value=0, max_value=5)
name = s.text(min_size=1, max_size=3, alphabet=string.ascii_lowercase)


def Vars(max_val: int = 10) -> s.SearchStrategy[Var]:
    return s.builds(Var, s.integers(min_value=0, max_value=max_val))


Consts = s.builds(Const, name)


def Vals(max_val: int = 10) -> s.SearchStrategy[Val]:
    return s.one_of(Vars(max_val), Consts)


def Tups(n: int, max_val: int = 10) -> s.SearchStrategy[Tup]:
    '''Generator for a tuple of a fixed length'''
    return s.builds(Tup, s.lists(Vals(max_val), min_size=n, max_size=n))


# Generator for a tuple of random length between 1 and 3
# And variable IDs ranging 0 to 10
Tups13 = n_1_3.flatmap(lambda n: Tups(n))


def Rels(n_attr: int = 2, n_row: int = 2) -> s.SearchStrategy[Rel]:
    return s.builds(Rel, name, s.lists(name, min_size=n_attr, max_size=n_attr,
                                       unique=True),
                    s.sets(Tups(n_attr), min_size=n_row, max_size=n_row))


# Relations with 1-3 attrs and 0-5 tuples
Rels1305 = n_1_3.flatmap(lambda n: n_0_5.flatmap(lambda m: Rels(n, m)))


def EgdTup(n: int) -> s.SearchStrategy[Tup]:
    '''
    Generator for tuple which has at least one 0 and one 1
    '''
    assert n >= 2, "Cannot make EgdTup for n < 2"
    return s.lists(s.integers(min_value=0, max_value=n - 1),
                   min_size=2, max_size=2, unique=True).flatmap(
        lambda inds: Tups(n).flatmap(
            lambda tup: s.just(Tup([
                Var(0) if i == inds[0] else (
                    Var(1) if i == inds[1] else x)
                for i, x in enumerate(tup)]))))


def Atoms(r: Rel) -> s.SearchStrategy[Atom]:
    return s.builds(Atom, s.just(r.name), Tups(len(r.attrs)))


@s.composite
def Matches(draw: Draw[Any], i: Inst) -> Match:
    assume(len(i))  # can only construct Match for a nonempty inst
    ind = draw(s.integers(min_value=0, max_value=len(i) - 1))
    atom = draw(Atoms(list(i)[ind]))
    allvars = list(atom.tup.variables)
    assume(allvars)
    var = s.sets(choice(allvars), min_size=1, max_size=len(allvars))
    return Match(atom, draw(var))


def Ands(i: Inst) -> s.SearchStrategy[And]:
    """Tree structure with base case of an And with two Matches"""
    return s.recursive(s.builds(And, Matches(i), Matches(i)),
                       lambda strat: s.builds(And, strat, strat))


# Generator for instances of 0 to 3 relations
Insts = s.builds(Inst, s.lists(Rels1305, min_size=0, max_size=3))


def Queries(i: Inst) -> s.SearchStrategy[Query]:
    '''Either a match or an And conjunction'''
    return s.one_of(Matches(i=i), Ands(i=i))


def EGDquery(i: Inst) -> s.SearchStrategy[Query]:
    '''
    A generator for queries that have at least 0 and 1
    in their quantified variables.
    '''
    raise NotImplementedError


# def TGDs = s.builds(TGD, Queries,)
def EGDs(i: Inst) -> s.SearchStrategy[EGD]:
    rel_candidates = [r for r in i if len(r.attrs) > 1]
    assert rel_candidates
    return choice(rel_candidates).flatmap(
        lambda r: s.builds(EGD, EGDquery(i))))


def Deps(i: Inst) -> s.SearchStrategy[Dependency]:
    return s.one_of(EGDs(i))


########
# Data #
########

i1_ = Inst.fromdict(
    A=(['col1', 'col2', 'col3'],
        [[1, 'x', 2], ['c', 'd', 3]]),
    B=(['col2', 'col4'],
       [['x', 'x'], ['x', 'y']]))

i1 = Inst.fromcsv(['B', 'A'])

iTriangle = Inst.fromcsv('Triangle')
iTriangle11 = Inst.fromcsv('TriangleLoop')

q1 = Match.fromlist('A', ['c', 4, 5])
q2 = Match.fromlist('B', ['x', 4])
q3 = And(q1, q2)

ab = i1['A'].join(i1['B'])
edge = Match.fromlist('Triangle', [0, 1])
egd_edge = EGD(edge)
egd_bidirectional = EGD(And(edge, Match.fromlist('Triangle', [1, 0])))
tgd = TGD.fromlist(Match.fromlist('TriangleLoop', [0, 0]), N=[1])


#########
# Tests #
#########
# IO
if False:
    def test_validate_fromcsv() -> None:
        '''Two equivalent ways of representing the same database'''
        assert i1 == i1_


# MISC
if False:
    @given(Vars)
    def test_isvar(v: Var) -> None:
        '''
        Trivial test. Demonstrates use of hypothesis.note
        To see the examples hypothesis generates, add a print stmt
        and disable pytest capturing with pytest -s test_chase.py.
        '''
        note('abs is %d' % abs(v.num))  # displayed ONLY when test fails
        assert v.isVar()


# ROUNDTRIP
if False:
    @given(Tups13)
    def test_tup_tolist_fromlist(t: Tup) -> None:
        assert t.fromlist(t.tolist()) == t

    @given(Insts)
    def test_inst_todict_fromdict(i: Inst) -> None:
        assert i.fromdict(**i.todict()) == i


# QUERIES
def test_validate_match() -> None:
    """Check whether ..."""
    res = Rel.fromlist([['x', 2], ['d', 3]], attrs=['4', '5'])
    assert q1.run(i1) == res
    res = Rel.fromlist([['x'], ['y']], attrs=['4'])
    assert q2.run(i1) == res


# DEPS
def test_fire() -> None:

    # Matching triangle for simple edges gives the same relation back
    res = egd_edge.q.run(iTriangle).rename_attrs(**{'0': 'src', '1': 'tar'})
    assert res.rename('Triangle') == iTriangle['Triangle']

    # Forcing all pairs (12, 23, 31) to be equal gives a single loop
    ires = egd_edge.fire(iTriangle)
    assert ires  # no error from firing constraint
    res = ires[0]['Triangle']
    assert len(res.tups) == 1  # only one edge
    t = list(res.tups)[0]
    assert t[0] == t[1]  # it's a self edge

    # There are no bidirectional edges in Triangle
    assert len(egd_bidirectional.q.run(iTriangle).tups) == 0

    # tgd.fire(iTriangle11))


@s.composite
def nonmatching_query(draw: Draw[Any]) -> T[Inst, Query]:
    inst: Inst = Inst([draw(Rels1305)])
    query: Query = draw(Queries(inst))
    assume(len(query.run(inst).tups) == 0)
    return inst, query


@given(nonmatching_query())
def test_fire_nonmatch_query(instquery: T[Inst, Query]) -> None:
    '''
    If you fire a dependency whose query does not yield any matches,
    then the result is identical to the initial database instance
    '''
    inst, query = instquery
    assert len(query.run(inst).tups) == 0


# @given(Insts, Deps)
# def test_fire_idempotent(i: Inst, d: Dependency) -> None:
#     pass
#     res = d.fire(i)
#     if res:
#         ires, _ = res
#         res2 = d.fire(ires)
#         if res2:
#             assert res2[0] == ires


if __name__ == '__main__':
    i = Inst([Rels1305.example()])
    x = Queries(i).example()
    breakpoint()
