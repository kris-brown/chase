# import pytest
from typing import (Any, Tuple as T, List as L, Callable as C, Union as U,
                    Set as S, FrozenSet as FS)
from chase import (Var, Tup, Const, Rel, Inst, EGD, TGD, Query,
                   Dependency, Atom, Val, V0, V1, ImmTup, MutTup,
                   Dependencies)
from hypothesis import assume, given, strategies as s
import string

"""
Run with python -m pytest test_chase.py
"""


################
# Example Data #
################

i1_ = Inst.fromdict(
    A=(['col1', 'col2', 'col3'],
        [[1, 'x', 2], ['c', 'd', 3]]),
    B=(['col2', 'col4'],
       [['x', 'x'], ['x', 'y']]))

i1 = Inst.fromcsv(['B', 'A'])

iTriangle = Inst.fromcsv('Triangle')
iTriangle11 = Inst.fromcsv('TriangleLoop')
iTriangle_N = Inst.union(
    iTriangle11, Inst([Rel('N', ['col1'])]))

q1 = Atom.fromlist('A', ['c', 4, 5])
q2 = Atom.fromlist('B', ['x', 4])
q3 = Query.fromatoms([q1, q2])


ab = i1['A'].join(i1['B'])
edge = Atom.fromlist('Triangle', [0, 1])
selfedge = Atom.fromlist('TriangleLoop', [0, 0])
egd_bidirectional = EGD(Query.fromatoms([edge, Atom.fromlist(
    'Triangle', [1, 0])]))

egd_edge = EGD(Query.fromatom(edge))

tgd = TGD.fromlist(Query.fromatom(selfedge), N=[0])


##############
# Generators #
##############

Draw = C[[s.SearchStrategy[Any]], Any]


def choice(xs: U[S[Any], L[Any]]) -> s.SearchStrategy[Any]:
    '''Randomly pick an element of a fixed list'''
    return s.one_of(*map(s.just, xs))


n_1_3 = s.integers(min_value=1, max_value=3)
n_0_5 = s.integers(min_value=0, max_value=5)
name = s.text(min_size=1, max_size=3, alphabet=string.ascii_lowercase)


def Vars(max_val: int = 3) -> s.SearchStrategy[Var]:
    return s.builds(Var, s.integers(min_value=0, max_value=max_val))


Consts = s.builds(Const, name)


def Vals(max_val: int = 3) -> s.SearchStrategy[Val]:
    return s.one_of(Vars(max_val), Consts)


def Tups(n: int, max_val: int = 3) -> s.SearchStrategy[Tup]:
    '''Generator for a tuple of a fixed length'''
    return s.builds(Tup, s.lists(Vals(max_val), min_size=n, max_size=n))


def ImmTups(n: int, max_val: int = 3) -> s.SearchStrategy[ImmTup]:
    return Tups(n, max_val).map(ImmTup.frombase)


def MutTups(n: int, max_val: int = 3) -> s.SearchStrategy[MutTup]:
    return Tups(n, max_val).map(MutTup.frombase)


# Generator for a tuple w/ len 1-3, variables 0-3
MutTups13 = n_1_3.flatmap(lambda n: MutTups(n))
ImmTups13 = n_1_3.flatmap(lambda n: MutTups(n))


def Rels(nAttr: int = 2, n_row: int = 2) -> s.SearchStrategy[Rel]:
    return s.builds(
        Rel, name, s.lists(name, min_size=nAttr, max_size=nAttr, unique=True),
        s.lists(MutTups(nAttr), min_size=n_row, max_size=n_row))


# Relations with 1-3 attrs and 0-5 tuples
Rels1305 = n_1_3.flatmap(lambda n: n_0_5.flatmap(lambda m: Rels(n, m)))

# Generator for instances of 0 to 3 relations
Insts = s.builds(Inst, s.lists(Rels1305, min_size=0, max_size=3,
                               unique_by=lambda r: r.name))


@s.composite
def iRels(draw: Draw, i: Inst) -> Rel:
    '''Get an arbitrary relation from a (assumed nonempty) inst'''
    assume(len(i))  # can only construct Match for a nonempty inst
    ind = draw(s.integers(min_value=0, max_value=len(i) - 1))
    return list(i)[ind]


def Atoms(r: Rel, max_var: int = 3) -> s.SearchStrategy[Atom]:
    '''Arbitrary atom given a particular relation'''
    return s.builds(Atom, s.just(r.name),
                    ImmTups(len(r.attrs), max_val=max_var))


def Atom_with_val(r: Rel, val: Val, mv: int = 3) -> s.SearchStrategy[Atom]:
    '''Create an atom that has AT LEAST a particular value'''
    n = len(r.attrs)
    assume(n)
    return s.integers(min_value=0, max_value=n - 1).flatmap(lambda i: Atoms(
        r, mv).map(lambda atom: Atom(atom.rel, atom.tup.replace(i, val))))


def iAtoms(i: Inst) -> s.SearchStrategy[Atom]:
    '''Arbitrary atom given a particular instance'''
    return iRels(i).flatmap(lambda r: Atoms(r))


def Queries(i: Inst) -> s.SearchStrategy[Query]:
    if len(i) == 0:
        return s.just(Query())  # only valid query on an empty inst
    matches = s.frozensets(iAtoms(i), min_size=0, max_size=3)
    return matches.flatmap(lambda ms: s.builds(
        Query, s.just(ms), (lambda var: s.frozensets(
            choice(var), min_size=0, max_size=len(var)))(
            set.union(*([x.tup.variables for x in ms] or [set()])))))


@s.composite
def nonmatching_query(draw: Draw) -> T[Inst, Query, Atom]:
    inst: Inst = Inst([draw(Rels1305)])  # create a random inst
    rel = draw(iRels(inst))  # pick a random relation
    n = len(rel.attrs)
    # Create a random CONSTANT tuple for this relation
    tup: ImmTup = draw(s.builds(ImmTup, s.lists(Consts, min_size=n, max_size=n
                                                ).map(tuple)))
    # check we didn't accidentally pick a const tuple actually present
    assume(tup not in rel.tups)
    qatom: Query = Query.fromatom(Atom(rel.name, tup))
    # Add it to a random query
    query: Query = draw(Queries(inst))
    return inst, qatom.merge(query), draw(iAtoms(inst))


@s.composite
def EGDs(draw: Draw, i: Inst) -> EGD:
    '''Generate an EGD by forcing a query to quantify over 0 and 1'''
    a01: FS[Atom] = frozenset([draw(Atom_with_val(draw(iRels(i)), Var(n)))
                               for n in range(2)])
    q = draw(Queries(i))
    return EGD(Query(frozenset.union(q.matches, a01), frozenset([V0, V1])))


def TGDs(i: Inst) -> s.SearchStrategy[TGD]:
    return (iRels(i)).flatmap(
        lambda r: s.builds(TGD, Queries(i), s.frozensets(iAtoms(i), min_size=1,
                                                         max_size=3)))


def Deps(i: Inst) -> s.SearchStrategy[Dependency]:
    '''Generate either an EGD or a TGD for a nonempty instance'''
    return s.one_of(EGDs(i), TGDs(i))


iDep = Insts.flatmap(lambda i: s.tuples(s.just(i), Deps(i)))
iEGD = Insts.flatmap(lambda i: s.tuples(s.just(i), EGDs(i)))
iDeps2 = Insts.flatmap(lambda i: s.tuples(
    s.just(i), s.builds(Dependencies, s.frozensets(Deps(i),
                        min_size=0, max_size=3))))


#########
# Tests #
#########

@given(Rels1305)
def test_rels_is_valid(r: Rel) -> None:
    '''Test whether generated rels are well-formed'''
    n = len(r.attrs)
    assert n == len(set(r.attrs))
    assert all([len(x) == n for x in r])


# IO
def test_validate_fromcsv() -> None:
    '''Two equivalent ways of representing the same database'''
    assert i1 == i1_


# ROUNDTRIP
@given(MutTups13)
def test_tup_tolist_fromlist(t: MutTup) -> None:
    assert t.fromlist(t.vals) == t


@given(Insts)
def test_inst_todict_fromdict(i: Inst) -> None:
    assert i.fromdict(**i.todict()) == i


def test_validate_match() -> None:
    """Check whether ..."""
    res = Rel.fromlist([['x', 2], ['d', 3]], attrs=['4', '5'])
    assert Query.fromatom(q1).run(i1) == res
    res = Rel.fromlist([['x'], ['y']], attrs=['4'])
    assert Query.fromatom(q2).run(i1) == res


# DEPS
def test_fire() -> None:

    # Matching triangle for simple edges gives the same relation back
    res = egd_edge.q.run(iTriangle)
    res.rename_attrs(**{'x0': 'src', 'x1': 'tar'})
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

    # Detect self loop
    assert Query.fromatom(selfedge).run(iTriangle11) == Rel.fromlist(
        [['a']], '', ['x0'])
    # Detect nothing in triangle w/o loop
    assert len(Query.fromatom(selfedge).rename(
        TriangleLoop='Triangle').run(iTriangle).tups) == 0

    # Get a single '1' added to relation N after firing
    res_ = tgd.fire(iTriangle_N)
    assert res_
    assert res_[0]['N'] == Rel.fromlist([['a']], 'N', ['col1'])


@given(nonmatching_query())
def test_fire_nonmatch_query(instquery: T[Inst, Query, Atom]) -> None:
    '''
    If you fire a dependency whose query does not yield any matches,
    then the result is identical to the initial database instance
    '''
    inst, query, atom = instquery
    if query.run(inst).tups:
        breakpoint()
    assert len(query.run(inst).tups) == 0

    tgd = TGD(query, frozenset([atom]))
    res = tgd.fire(inst)
    assert res
    assert res[0] == inst


@given(iEGD)
def test_egd_idempotent(idep: T[Inst, EGD]) -> None:
    '''
    Equality-generating dependencies ought not change an
    instance when executed a second time in a row
    '''
    i, d = idep
    res = d.fire(i)  # first time we run, might be different from 'i'
    if res:
        ires, _ = res
        res2 = d.fire(ires)  #
        assert res2
        assert res2[0] == ires


def test_chase() -> None:

    # If there's an edge a->b, then there's an edge b->a
    deps = Dependencies.fromlist([
        TGD.fromlist(Query.fromatom(edge), Triangle=[1, 0])
    ])

    res = deps.chase(iTriangle)
    assert res.fail is None and not res.timeout
    assert len(res) == 2


if __name__ == '__main__':
    pass
