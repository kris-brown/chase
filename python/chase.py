from typing import (Any, List as L, Set as S, Iterator as I, Dict as D,
                    Optional as O, Tuple as T, Union as U, FrozenSet as FS)
from abc import ABCMeta, abstractmethod
from prettytable import PrettyTable
from string import ascii_lowercase
from dataclasses import dataclass, field
import os
import csv
import copy
import random
import pathlib


@dataclass(order=True, frozen=True)
class Var:
    '''Variables, indexed by integers'''
    num: int

    def __str__(self) -> str:
        return str(self.num)

    def cmpstr(self) -> str:
        return '0%d' % self.num


@dataclass(order=True, frozen=True)
class Const:
    '''Stand-in type for all concrete values'''
    value: str

    def __str__(self) -> str:
        return self.value

    def cmpstr(self) -> str:
        return '1' + self.value


V0, V1 = Var(0), Var(1)
Val = U[Var, Const]


class Tup(metaclass=ABCMeta):
    '''Relational tuples, i.e. sequences of Vars or Consts'''

    def __init__(self, vals: U[L[Val], T[Val, ...]]) -> None:
        self.vals = vals

    def __len__(self) -> int:
        return len(self.vals)

    def __lt__(self, other: object) -> bool:
        assert isinstance(other, Tup)
        return [v.cmpstr() for v in self] < [v.cmpstr() for v in other]

    def __str__(self) -> str:
        return '(%s)' % ','.join(map(str, self.vals))

    def __iter__(self) -> I[Val]:
        return iter(self.vals)

        return hash(tuple(self.vals))

    def __getitem__(self, key: int) -> Val:
        return self.vals[key]

    @property
    def variables(self) -> S[Var]:
        return set([x for x in self.vals if isinstance(x, Var)])

    @property
    def hash(self) -> int:
        return hash(tuple(self.vals))

    @classmethod
    def fromlist(cls, xs: L[Any]) -> 'Tup':
        '''
        Take a list of strings, interpret int-like strings as Variables
        '''
        def f(x: Any) -> Val:
            if isinstance(x, (Var, Const)):
                return x
            elif isinstance(x, str):
                return Const(x)
            elif isinstance(x, int):
                return Var(x)
            else:
                raise TypeError(type(x))
        return cls([f(x) for x in xs])


@dataclass()
class MutTup(Tup):
    '''Mutable relational tuple'''
    vals: L[Val]

    @classmethod
    def fromlist(cls, xs: L[Any]) -> 'MutTup':
        '''Use the base class Tup method to make a mutable one'''
        return cls.frombase(super(cls, cls).fromlist(xs))

    def substitute(self, var: Var, val: Val) -> None:
        '''Modify tuple, replacing a variable with a value'''
        for i in range(len(self)):
            if self[i] == var:
                self.vals[i] = val

    @classmethod
    def frombase(cls, t: Tup) -> 'MutTup':
        return cls(list(t.vals))


@dataclass(frozen=True)
class ImmTup(Tup):
    '''Immutable relational tuple'''
    vals: T[Val, ...]

    @classmethod
    def fromlist(cls, xs: L[Any]) -> 'ImmTup':
        '''Use the base class Tup method to make an immutable one'''
        return cls.frombase(super(cls, cls).fromlist(xs))

    @classmethod
    def frombase(cls, t: Tup) -> 'ImmTup':
        return cls(tuple(t.vals))

    def replace(self, i: int, v: Val) -> 'ImmTup':
        '''Creates a new copy'''
        return ImmTup(tuple(v if i == j else x for j, x in enumerate(self)))


@dataclass(order=True)
class Rel:
    '''
    A named relation with named attributes.
    Tuple values are strings or int-indexed variables
    '''
    name: str = ''
    attrs: L[str] = field(default_factory=list)
    tups: L[MutTup] = field(default_factory=list)

    def __post_init__(self) -> None:
        n = len(self.attrs)
        assert n == len(set(self.attrs))
        assert all([len(x) == n for x in self])

    def __str__(self) -> str:
        t = PrettyTable(self.attrs)
        for tup in self.tups:
            t.add_row(tup)
        return '{}\n{}'.format(self.name, t)

    def __eq__(self, other: object) -> bool:
        '''
        We need custom EQ method because we do
        not care about the ORDER of the rels
        '''
        if isinstance(other, Rel):
            if self.name == other.name:
                if sorted(other.attrs) == sorted(self.attrs):
                    return sorted(self.tups) == sorted(
                        other.permute(self.attrs).tups)
        return False

    def __iter__(self) -> I[MutTup]:
        return iter(self.tups)

    # CREATE
    @classmethod
    def fromcsv(cls, pth: str) -> 'Rel':
        '''
        Interprets integer-like strings as variables

        Tries to interpret the path as just the name of the csv file
        But defaults to a generic filepath otherwise.
        '''
        def parse_int(x: str) -> U[str, int]:
            return int(x) if x.isdigit() else x

        rows = []
        data = pathlib.Path(__file__).parent.parent.absolute()
        just_name = '%s/data/%s.csv' % (data, pth)
        fi = just_name if os.path.exists(just_name) else pth
        with open(fi, 'r') as f:
            reader = csv.reader(f)
            headers = next(reader)
            for row in reader:
                rows.append(list(map(parse_int, row)))
        return cls(pth, headers, list(map(MutTup.fromlist, rows)))

    @classmethod
    def fromlist(cls, xs: L[L[Any]], name: str = '', attrs: L[str] = None
                 ) -> 'Rel':
        '''
        Create Relation from a list of tuples
        Optionally give relation and attr names
        '''
        tups = [MutTup.fromlist(x) for x in xs]
        if attrs is None:
            if not xs:
                attrs = []
            else:
                attrs = list(ascii_lowercase[:len(xs[0])])

        if xs:
            assert len(attrs) == len(xs[0]), "Gave attr list with wrong #"

        return cls(name, attrs, tups)

    # PROPERTIES
    @property
    def trivial(self) -> bool:
        return len(self.attrs) == 0 and not self.empty

    @property
    def empty(self) -> bool:
        return len(self.tups) == 0

    @property
    def max_var(self) -> int:
        '''
        Greatest index of a variable in the relation
        Perhaps this can be stored and only updated if a tuple is added
        '''
        return max([max([v.num for v in t.variables] or [-1]
                        ) for t in self] or [-1])

    def rename(self, name: str) -> 'Rel':
        '''Create a copy'''
        return Rel(name, self.attrs, self.tups)

    def permute(self, col_order: L[str]) -> 'Rel':
        '''Create a copy with columns rearranged or removed'''
        inds = [self.attrs.index(x) for x in col_order]
        out = copy.deepcopy(self)
        out.attrs = col_order
        for j in range(len(out.tups)):
            out.tups[j] = MutTup([out.tups[j][i] for i in inds])
        return out

    # MODIFY (do we need remove duplicates method, renormalize variables?)
    def clean(self) -> None:
        '''Remove duplicates'''
        seen: S[int] = set()
        for t in list(self):
            h = t.hash
            if h in seen:
                self.tups.remove(t)
            else:
                seen.add(h)

    def join(self, R: 'Rel', save_overlap: bool = False) -> 'Rel':
        '''
        "Nested loop join" algorithm
        Join two relations based on their attribute names
        Optionally keep the overlapping attributes in the result
        '''

        overlap = set(self.attrs) & set(R.attrs)
        inds = [(self.attrs.index(x), R.attrs.index(x)) for x in overlap]
        selfinds = [i for i in range(len(self.attrs))
                    if save_overlap or self.attrs[i] not in overlap]
        Rinds = [i for i in range(len(R.attrs)) if R.attrs[i] not in overlap]

        # Give meaningful name iff the joined relations have names
        name = self.name + ' ⋈ ' + R.name if (self.name + R.name) else ''

        res = []
        for tup in self:
            for Rtup in R:
                if all([tup[i] == Rtup[j] for i, j in inds]):
                    res.append(MutTup([tup[i] for i in selfinds] + [
                        Rtup[i] for i in Rinds]))

        result = Rel(name, [self.attrs[i] for i in selfinds
                            ] + [R.attrs[i] for i in Rinds], res)
        result.clean()
        # if result.attrs == ['x0', 'x1']:
        #     breakpoint()
        return result

    def substitute(self, var: Var, val: Val) -> None:
        '''Modify relation, replacing a variable with a value'''
        for t in self:
            t.substitute(var, val)

    def substitutes(self, d: D[Var, Val]) -> None:
        for k, v in d.items():
            self.substitute(k, v)

    def rename_attrs(self, **kwargs: str) -> None:
        '''Rename with a dictionary of old_col -> new_col pairs'''
        err = '%s not found in %s\'s attrs %s'
        for k in kwargs:
            assert k in self.attrs, err % (k, self.name, self.attrs)
        self.attrs = [kwargs.get(x, x) for x in self.attrs]

    def rename_attr(self, old: str, new: str) -> None:
        self.rename_attrs(**{old: new})


trivial = Rel(tups=[MutTup([])])  # trivially true query result


@dataclass(eq=True)
class Inst:
    rels: L[Rel]

    def __post_init__(self) -> None:
        n = [r.name for r in self]
        assert len(n) == len(set(n))
        self.rels = sorted(self.rels)
        self.clean()

    def __eq__(self, other: object) -> bool:
        '''
        We need custom EQ method because we do
        not care about the ORDER of the rels
        '''
        if isinstance(other, Inst):
            return sorted(self.rels) == sorted(other.rels)
        else:
            return False

    def __iter__(self) -> I[Rel]:
        return iter(self.rels)

    def __len__(self) -> int:
        return len(self.rels)

    def __str__(self) -> str:
        return '\n'.join(map(str, self.rels))

    def __getitem__(self, key: str) -> Rel:
        for r in self:
            if r.name == key:
                return r
        raise KeyError(key)

    @property
    def max_var(self) -> int:
        return max([r.max_var for r in self]) if len(self) > 0 else -1

    @classmethod
    def fromdict_noattr(cls, **kwargs: L[L[Any]]) -> 'Inst':
        return cls([Rel.fromlist(v, k) for k, v in kwargs.items()])

    @classmethod
    def union(cls, *xs: 'Inst') -> 'Inst':
        return cls([rel for rels in xs for rel in rels])

    @classmethod
    def fromdict(cls, **kwargs: T[L[str], L[L[Any]]]) -> 'Inst':
        return cls([Rel.fromlist(y, k, x) for k, (x, y) in kwargs.items()])

    def todict(self) -> D[str, T[L[str], L[L[Val]]]]:
        def f(r: Rel) -> T[L[str], L[L[Val]]]:
            return list(r.attrs), [t.vals for t in r.tups]
        return {r.name: f(r) for r in self}

    @classmethod
    def fromcsv(cls, pth: U[str, L[str]]) -> 'Inst':
        pths = [pth] if isinstance(pth, str) else pth
        return cls([Rel.fromcsv(p) for p in pths])

    @classmethod
    def empty(cls) -> 'Inst':
        return cls([])

    @property
    def names(self) -> S[str]:
        return set(map(lambda r: r.name, self))

    def clean(self) -> None:
        for r in self:
            r.clean()


@dataclass(order=True, frozen=True)
class Atom:
    '''A tuple paired with the name of a relation it belongs to'''
    rel: str
    tup: ImmTup

    def __len__(self) -> int:
        return len(self.tup)

    def __getitem__(self, i: int) -> Val:
        return self.tup[i]

    def __iter__(self) -> I[Val]:
        return iter(self.tup)

    def __str__(self) -> str:
        return '{%s|%s}' % (self.rel, self.tup)

    @classmethod
    def fromlist(cls, rel: str, tup: L[Any]) -> 'Atom':
        return cls(rel, ImmTup.fromlist(tup))

    def match_rel(self, inst: Inst, dist: FS[Var]) -> Rel:
        '''
        Match a conjunct to a relation
        Keep track of max variable index
        '''
        target = inst[self.rel]

        # The columns in the result are the distinguished
        # variables of the conjunctive query that are also
        # found in this conjunct
        cols, tups = [x for x in sorted(dist) if x in self.tup], []

        # Process each tuple in the relation and see
        # if it matches the pattern of this conjunct
        for tup in target:
            tup_res = self.match_tup(tup)
            if tup_res:
                # if it does, record what values the
                # distinguished variables matched to
                tups.append(MutTup([tup_res[v] for v in cols]))

        result = Rel('', ['x%d' % x.num for x in cols], tups)
        result.clean()
        return result

    def match_tup(self, target: Tup) -> O[D[Var, Val]]:
        '''Match tuple of a conjunct to the right relation'''
        res: D[Var, Val] = {}  # mapping of variables to values

        def process_var_val(var: Var, val: Val) -> bool:
            '''
            Update mapping and return true if there
            is inconsistent assignment.
            '''
            if var in res and res[var] != val:
                return True  # inconsistant assignment
            res[var] = val   # update mapping
            return False

        # Process the tuple and short-circuit if
        # there is an inconsistent mapping
        for qval, val in zip(self.tup, target):
            fail = False
            if isinstance(qval, Var):
                fail = process_var_val(qval, val)
            elif qval != val:  # differing consts
                return None
            if fail:
                return None
        return res


@dataclass(order=True, frozen=True)
class Query:
    '''
    Conjunctive query. A logical constraint with variables which, when the
    query is satisfied, get matched to values.

    dist indicates which variables are distinguished (the ones we care about)

    Running the query returns all satisfying matches.
    '''
    matches: FS[Atom] = field(default_factory=frozenset)
    dist: FS[Var] = field(default_factory=frozenset)

    def __post_init__(self) -> None:
        for d in self.dist:
            assert d in self.matchvars

    @property
    def matchvars(self) -> FS[Var]:
        return frozenset(set.union(*([x.tup.variables
                                      for x in self] or [set()])))

    def __str__(self) -> str:
        if len(self) == 0:
            return '𝗧'
        elif len(self) == 1:
            return str(next(iter(self)))
        else:
            body = '\n'.join([str(x) for x in self])
            return '***\n%s\n***' % body

    def __iter__(self) -> I[Atom]:
        return iter(self.matches)

    def __len__(self) -> int:
        return len(self.matches)

    def db(self) -> Inst:
        '''Create canonical database associated with the query'''
        raise NotImplementedError

    def run(self, i: Inst) -> Rel:
        res = Rel(tups=[MutTup([])])
        for conjunct in self:
            res_ = conjunct.match_rel(i, self.dist)
            res = res.join(res_, save_overlap=True)
        return res

    @classmethod
    def fromatoms(cls, xs: L[Atom], dist: L[Var] = None) -> 'Query':
        '''Default to all variables mentioned as distinguished'''
        dist_ = frozenset(dist if dist else set.union(*[
            x.tup.variables for x in xs]))
        return cls(frozenset(xs), dist_)

    @classmethod
    def fromatom(cls, x: Atom, dist: L[Var] = None) -> 'Query':
        return cls.fromatoms([x], dist)

    @classmethod
    def fromlist(cls, rel: str, tup: L[Any]) -> 'Query':
        '''Declare a query from a single conjunct'''
        return cls(frozenset([Atom.fromlist(rel, tup)]),
                   frozenset([Var(t) for t in tup if isinstance(t, int)]))

    @classmethod
    def fromlists(cls, **kwargs: L[Any]) -> 'Query':
        '''Declare multiple conjuncts together'''
        conj = frozenset([Atom.fromlist(rel, tup)
                          for rel, tup in kwargs.items()])
        dist = set.union(*[c.tup.variables for c in conj])
        return cls(conj, frozenset(dist))

    def rename(self, **kwargs: str) -> 'Query':
        return Query(frozenset([Atom(kwargs.get(a.rel, a.rel), a.tup)
                                for a in self.matches]), self.dist)

    def merge(self, q: 'Query') -> 'Query':
        return Query(frozenset.union(self.matches, q.matches),
                     frozenset.union(self.dist, q.dist))


class Dependency(metaclass=ABCMeta):
    '''Sum type of EGD and TGD'''

    def fire(self, i: Inst, counter: int = None) -> O[T[Inst, int]]:
        cntr = counter or i.max_var + 1
        res = self._fire(i, cntr)
        if res:
            res[0].clean()
            return res
        return None

    @abstractmethod
    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        raise NotImplementedError


@dataclass(order=True, frozen=True)
class EGD(Dependency):
    '''
    Equality-generating dependency with the form Ψ(x,y) -> x=y

    If the query is matched, then the variables 0 and 1 are
    indicated to be equal.
    '''
    q: Query

    def __post_init__(self) -> None:
        assert frozenset([V0, V1]).issubset(self.q.dist)

    def __str__(self) -> str:
        return '{} → 0 = 1'.format(self.q)

    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        i = copy.deepcopy(i)
        res: L[T[Val, Val]] = [(x, y) for x, y in self.q.run(i).tups]
        rep: D[Var, Val] = dict()  # representative of equiv class
        for a0, a1 in res:
            if isinstance(a0, Var):
                if isinstance(a1, Var):
                    if a0 in rep:
                        if a1 in rep:
                            t0, t1 = rep[a0], rep[a1]
                            if t0 != t1:
                                # BOTH vars are mapped to different things
                                if isinstance(t0, Var):
                                    rep[t0] = t1
                                    for key in rep:
                                        if rep[key] == t0:
                                            rep[key] = t1
                                elif isinstance(t1, Var):
                                    rep[t1] = t0
                                    for key in rep:
                                        if rep[key] == t1:
                                            rep[key] = t0
                                else:
                                    return None

                            else:
                                pass  # nothing to do
                        else:
                            # point a1 to wherever a0 was pointing
                            rep[a1] = rep[a0]
                    else:
                        if a1 in rep:
                            # point a0 to wherever a1 was pointing
                            rep[a0] = rep[a1]
                        else:
                            # Neither var is mapped, arbitrarily pick
                            rep[a0] = a1
                else:
                    # a0 is var, a1 in const
                    if a0 in rep and rep[a0] != a1:
                        return None
                    else:
                        rep[a0] = a1
            else:
                if isinstance(a1, Var):
                    # a0 is const, a1 is var
                    if a1 in rep and rep[a1] != a0:
                        return None
                    else:
                        rep[a1] = a0
                else:
                    # two consts: fail if they're supposed to be equal
                    if a0 != a1:
                        return None
                    else:
                        pass
        for rel in i:
            for var, val in rep.items():
                rel.substitute(var, val)
        return i, counter


@dataclass(order=True, frozen=True)
class TGD(Dependency):
    '''
    Tuple-generating dependency. For some variables that match a constraint
    (the antecedent query), we demand there exist certain atoms (involving
    those variables, plus others which are implicitly quantified over via ∃).
    '''
    q: Query
    exists: FS[Atom] = field(default_factory=frozenset)

    def __str__(self) -> str:
        return '{} → ∃ {}'.format(
            self.q, ', '.join(['{}:{}'.format(atom.rel, atom.tup)
                               for atom in self.exists]))

    @classmethod
    def fromlist(cls, q: Query, **exists: L[Any]) -> 'TGD':
        return cls(q, frozenset([Atom(k, ImmTup.fromlist(v))
                                 for k, v in exists.items()]))

    @property
    def exists_vars(self) -> S[Var]:
        return set.union(*[x.tup.variables for x in self.exists]) - self.q.dist

    @property
    def bound_vars(self) -> S[Var]:
        return set.union(*[x.tup.variables for x in self.exists]) & self.q.dist

    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        c = [counter]

        def new(cntr: L[int]) -> Var:
            c[0] += 1
            return Var(c[0])

        i = copy.deepcopy(i)
        v = sorted(self.q.dist)
        for res in self.q.run(i):
            dic = dict(zip(v, res))
            for atom in self.exists:
                newtup = [dic.get(val, new(c)) if isinstance(val, Var) else val
                          for val in atom.tup]
                i[atom.rel].tups.append(MutTup(newtup))

        return i, c[0]


@dataclass(order=True, frozen=True)
class ChaseResult():
    init: Inst
    sequence: T[T[Dependency, Inst], ...] = ()
    timeout: bool = False
    fail: O[Dependency] = None

    def __len__(self) -> int:
        return len(self.sequence)

    def __iter__(self) -> I[T[Dependency, Inst]]:
        return iter(self.sequence)

    def result(self) -> O[Inst]:
        if not self.timeout and not self.fail:
            return self.sequence[-1][1]
        else:
            return None


@dataclass(order=True, frozen=True)
class Dependencies():
    '''Methods depending on sets of dependencies'''

    deps: FS[Dependency] = field(default_factory=frozenset)

    @classmethod
    def fromlist(cls, deps: L[Dependency]) -> 'Dependencies':
        return cls(frozenset(deps))

    def chase(self, i: Inst, limit: int = 100) -> ChaseResult:
        '''
        Naive chase algorithm with a step limit
        to prevent nontermination

        Fire dependencies randomly until convergence.
        '''
        init, i_init, i_prev = True, i, i
        sequence: L[T[Dependency, Inst]] = []

        while init or i != i_prev:
            init = False
            i_prev = i
            max_var = i.max_var
            deps = list(self.deps)
            random.shuffle(deps)
            for dep in deps:
                if len(sequence) > limit:
                    return ChaseResult(i_init, tuple(sequence), True)

                res = dep.fire(i, max_var)
                if res:
                    i, max_var = res
                    sequence.append((dep, i))

                else:
                    return ChaseResult(i_init, tuple(sequence), fail=dep)
        return ChaseResult(i_init, tuple(sequence))


def main() -> None:
    pass


if __name__ == '__main__':
    main()
