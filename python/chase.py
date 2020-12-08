from typing import (Any, List as L, Set as S, Iterator as I, Dict as D,
                    Optional as O, Tuple as T, Union as U)
from abc import ABCMeta, abstractmethod
from prettytable import PrettyTable
from string import ascii_lowercase
import os
import csv
import copy
import pathlib


class Val(metaclass=ABCMeta):
    @abstractmethod
    def isVar(self) -> bool:
        raise NotImplementedError

    @property
    def dtype(self) -> str:
        return 'str'  # add support for other dtypes later

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


class Var(Val):
    def __init__(self, num: int) -> None:
        self.num = num

    def __str__(self) -> str:
        return str(self.num)

    __repr__ = __str__

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Var) and self.num == other.num

    def __lt__(self, other: Any) -> bool:
        return isinstance(other, Var) and self.num < other.num

    def __hash__(self) -> int:
        return hash(self.num)

    def isVar(self) -> bool:
        return True


class Const(Val):
    def __init__(self, value: str) -> None:
        self.value = value

    def __str__(self) -> str:
        return str(self.value)

    __repr__ = __str__

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Const) and self.value == other.value

    def __lt__(self, other: Any) -> bool:
        return isinstance(other, Const) and self.value < other.value

    def __hash__(self) -> int:
        return hash(self.value)

    def isVar(self) -> bool:
        return False


class Tup(object):
    def __init__(self, vals: L[Val]) -> None:
        self.vals = tuple(vals)

    def __len__(self) -> int:
        return len(self.vals)

    def __str__(self) -> str:
        return str(self.vals)

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Tup) and self.vals == other.vals

    def __iter__(self) -> I[Val]:
        return iter(self.vals)

    def __getitem__(self, key: int) -> Val:
        return self.vals[key]

    def __hash__(self) -> int:
        return hash(self.vals)

    @property
    def variables(self) -> S[Var]:
        return set([x for x in self.vals if isinstance(x, Var)])

    @property
    def sig(self) -> L[str]:
        '''Signature of tuple, giving the types of each arg'''
        return [x.dtype for x in self]

    @classmethod
    def fromlist(cls, xs: L[Any]) -> 'Tup':
        def f(x: Any) -> Val:
            if isinstance(x, Val):
                return x
            elif isinstance(x, str):
                return Const(x)
            elif isinstance(x, int):
                return Var(x)
            else:
                raise TypeError(type(x))
        return cls([f(x) for x in xs])

    def tolist(self) -> L[Val]:
        return list(self.vals)

    def substitute(self, var: Var, val: Val) -> 'Tup':
        '''Modify tuple, replacing a variable with a value'''
        if var in self:
            return Tup([val if v == var else v for v in self.vals])
        else:
            return self


class Rel(object):
    '''
    A named relation with named attributes.
    Tuple values are strings or int-indexed variables
    '''
    def __init__(self, name: str, attrs: L[str], tups: S[Tup]) -> None:
        assert len(attrs) == len(set(attrs)), '''Column headers must be unique
             %s''' % attrs
        assert isinstance(tups, set)
        assert isinstance(attrs, list)
        self.name = name
        self.attrs = tuple(attrs)
        self.tups = tups

    def __str__(self) -> str:
        t = PrettyTable(self.attrs)
        for tup in self.tups:
            t.add_row(tup)
        return '{}\n{}'.format(self.name, t)

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Rel) and vars(self) == vars(other)

    def __iter__(self) -> I[Tup]:
        return iter(self.tups)

    # CREATE
    @classmethod
    def empty(cls, name: str, attrs: L[str]) -> 'Rel':
        return cls(name, attrs, set())

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
        return cls(pth, headers, set(map(Tup.fromlist, rows)))

    @classmethod
    def fromlist(cls, xs: L[L[Any]], name: str = '', attrs: L[str] = None
                 ) -> 'Rel':
        '''
        Create Relation from a list of tuples
        Optionally give relation and attr names
        '''
        assert xs, "Gave empty relation to Rel.fromlist"
        tups = [Tup.fromlist(x) for x in xs]
        assert len(set([str(t.sig) for t in tups])) == 1
        attrs = attrs or list(ascii_lowercase[:len(xs[0])])
        assert len(attrs) == len(xs[0]), "Gave attr list with wrong #"
        return cls(name, attrs, set(tups))

    # PROPERTIES
    @property
    def max_var(self) -> int:
        '''
        Greatest index of a variable in the relation

        Perhaps this can be stored and only updated if a tuple is added
        '''
        return max([max([v.num for v in t.variables] or [0]) for t in self])

    # UPDATE STATE
    def add(self, t: Tup) -> None:
        self.tups.add(t)

    # MODIFY
    def join(self, R: 'Rel', save_overlap: bool = False) -> 'Rel':
        '''
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

        res = Rel(name, [self.attrs[i] for i in selfinds
                         ] + [R.attrs[i] for i in Rinds], set())
        for tup in self:
            for Rtup in R:
                if all([tup[i] == Rtup[j] for i, j in inds]):
                    res.add(Tup([tup[i] for i in selfinds] + [
                        Rtup[i] for i in Rinds]))

        return res

    def substitute(self, var: Var, val: Val) -> None:
        '''Modify relation, replacing a variable with a value'''
        old = list(self)
        self.tups = set()
        for t in old:
            self.add(t.substitute(var, val))

    def rename(self, name: str) -> 'Rel':
        return Rel(name, list(self.attrs), self.tups)

    def rename_attrs(self, **kwargs: str) -> 'Rel':
        '''Rename with a dictionary of old_col-> new_col pairs'''
        err = '%s not found in %s\'s attrs %s'
        for k in kwargs:
            assert k in self.attrs, err % (k, self.name, self.attrs)
        attrs = [kwargs.get(x, x) for x in self.attrs]
        return Rel(self.name, attrs, self.tups)

    def rename_attr(self, old: str, new: str) -> 'Rel':
        return self.rename_attrs(**{old: new})


class Inst(object):
    def __init__(self, rels: L[Rel]) -> None:
        self.rels = {r.name: r for r in rels}

    def __iter__(self) -> I[Rel]:
        return iter(self.rels.values())

    def __len__(self) -> int:
        return len(self.rels)

    def __str__(self) -> str:
        return '\n'.join(map(str, self.rels.values()))

    __repr__ = __str__

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Inst) and self.rels == other.rels

    def __getitem__(self, key: str) -> Rel:
        for r in self:
            if r.name == key:
                return r
        raise KeyError

    @property
    def max_var(self) -> int:
        return max([r.max_var for r in self]) if len(self) > 0 else 0

    @classmethod
    def fromdict_noattr(cls, **kwargs: L[L[Any]]) -> 'Inst':
        return cls([Rel.fromlist(v, k) for k, v in kwargs.items()])

    @classmethod
    def fromdict(cls, **kwargs: T[L[str], L[L[Any]]]) -> 'Inst':
        return cls([Rel.fromlist(y, k, x) for k, (x, y) in kwargs.items()])

    def todict(self) -> D[str, T[L[str], L[L[Val]]]]:
        def f(r: Rel) -> T[L[str], L[L[Val]]]:
            return list(r.attrs), [t.tolist() for t in r.tups]
        return {r.name: f(r) for r in self}

    @classmethod
    def fromcsv(cls, pth: U[str, L[str]]) -> 'Inst':
        pths = [pth] if isinstance(pth, str) else pth
        return cls([Rel.fromcsv(p) for p in pths])

    @property
    def names(self) -> S[str]:
        return set(map(lambda r: r.name, self))


class Atom(object):
    def __init__(self, rel: str, tup: Tup) -> None:
        self.rel = rel
        self.tup = tup


class Query(metaclass=ABCMeta):
    '''
    Conjunctive query. A logical constraint with variables that, when the
    query is satisfied, get matched to values.
    Running the query returns all satisfying matches.
    '''
    @property
    @abstractmethod
    def var(self) -> S[Var]:
        raise NotImplementedError

    @abstractmethod
    def run(self, i: Inst) -> Rel:
        raise NotImplementedError

    def __str__(self) -> str:
        return self.show()

    @abstractmethod
    def show(self) -> str:
        raise NotImplementedError

    def db(self) -> Inst:
        '''Create canonical database associated with the query'''
        raise NotImplementedError


class Match(Query):
    '''
    An atom with designated variables that are quantified over.
    '''
    def __init__(self, atom: Atom, var: S[Var] = None) -> None:
        self.atom = atom
        self._var = atom.tup.variables if var is None else var
        assert self._var
        assert all([v in atom.tup for v in self._var])

    def __eq__(self, other: Any) -> bool:
        return isinstance(other, Match) and vars(self) == vars(other)

    def show(self) -> str:
        return '{{{}| ∀{}:{}}}'.format(
            self.atom.rel, ','.join(map(str, self._var)),
            self.atom.tup)

    @classmethod
    def fromlist(cls, rel: str, tup: L[Any]) -> 'Match':
        return cls(Atom(rel, Tup.fromlist(tup)))

    @property
    def var(self) -> S[Var]:
        return self._var

    def run(self, i: Inst) -> Rel:
        assert self.atom.rel in i.names
        R = i[self.atom.rel]
        return self.match_rel(R)

    def match_rel(self, target: Rel) -> Rel:
        res = Rel('', list(map(str, self._var)), set())

        for tup in target:
            tupres = self.match_tup(tup)
            if tupres:
                res.add(Tup([tupres[v] for v in self._var]))
        return res

    def match_tup(self, target: Tup) -> O[D[Var, Val]]:
        res: D[Var, Val] = {}

        def process_var_val(var: Var, val: Val) -> bool:
            if var in res and res[var] != val:
                return True  # inconsistant
            res[var] = val
            return False

        for qval, val in zip(self.atom.tup, target):
            fail = False
            if isinstance(qval, Var):
                fail = process_var_val(qval, val)
            elif isinstance(val, Var):
                fail = process_var_val(val, qval)
            elif qval != val:  # both are const
                return None
            if fail:
                return None
        return res


class And(Query):
    def __init__(self, q1: Query, q2: Query) -> None:
        self.q1 = q1
        self.q2 = q2

    def show(self) -> str:
        return '{}\nAND\n{}'.format(self.q1.show(), self.q2.show())

    @property
    def var(self) -> S[Var]:
        return self.q1.var | self.q2.var

    def run(self, i: Inst) -> Rel:
        R1 = self.q1.run(i)
        R2 = self.q2.run(i)
        return R1.join(R2, save_overlap=True)


class Dependency(metaclass=ABCMeta):
    '''Sum type of EGD and TGD'''
    def fire(self, i: Inst, counter: int = None) -> O[T[Inst, int]]:
        cntr = counter or i.max_var + 1
        return self._fire(i, cntr)

    @abstractmethod
    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        raise NotImplementedError


class EGD(Dependency):
    '''
    Equality-generating dependency with the form Ψ(x,y) -> x=y

    If the query is matched, then the variables 0 and 1 are
    indicated to be equal.
    '''
    def __init__(self, q: Query) -> None:
        self.q = q
        assert q.var == set([Var(0), Var(1)])

    def __str__(self) -> str:
        return '{} → 0 = 1'.format(self.q)

    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        i = copy.deepcopy(i)
        pairs: L[T[Var, Val]] = []
        for a0, a1 in sorted([sorted(x) for x in self.q.run(i)]):
            if isinstance(a0, Var):
                pairs.append((a0, a1))
            elif isinstance(a1, Var):
                pairs.append((a1, a0))
            elif a0 != a1:
                return None  # two unequal constants

        for rel in i:
            for var, val in pairs:
                rel.substitute(var, val)
        return i, counter


class TGD(Dependency):
    '''
    Tuple-generating dependency. For some variables that match a constraint
    (the antecedent query), we demand there exist certain atoms (involving
    those variables, plus others which are implicitly quantified over via ∃).
    '''
    def __init__(self, q: Query, exists: S[T[str, Tup]]) -> None:
        self.q = q
        self.exists = exists

    def __str__(self) -> str:
        return '{} → ∃ {}'.format(
            self.q, ', '.join(['{}:{}'.format(k, v) for k, v in self.exists]))

    @classmethod
    def fromlist(cls, q: Query, **exists: L[Any]) -> 'TGD':
        return cls(q, set([(k, Tup.fromlist(v)) for k, v in exists.items()]))

    @property
    def exists_vars(self) -> S[Var]:
        return set.union(*[x.variables for _, x in self.exists]) - self.q.var

    @property
    def bound_vars(self) -> S[Var]:
        return set.union(*[x.variables for _, x in self.exists]) & self.q.var

    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        c = [counter]

        def new(cntr: L[int]) -> Var:
            c[0] += 1
            return Var(c[0])

        i = copy.deepcopy(i)
        v = sorted(self.q.var)
        for res in self.q.run(i):
            dic = dict(zip(v, res))
            for relname, tup in self.exists:
                newtup = [dic.get(val, new(c)) if isinstance(val, Var) else val
                          for val in tup]
                i[relname].add(Tup(newtup))

        return i, c[0]


class Dependencies(object):
    '''Methods depending on sets of dependencies'''

    def __init__(self, deps: S[Dependency]) -> None:
        self.deps = deps

    def chase(self, i: Inst) -> Inst:
        raise NotImplementedError


def main() -> None:
    pass


if __name__ == '__main__':
    main()
