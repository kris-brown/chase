from typing import (Any, List as L, Set as S, Iterator as I, Dict as D,
                    Optional as O, Tuple as T)
from abc import ABCMeta, abstractmethod
from prettytable import PrettyTable
from string import ascii_lowercase
import copy


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
    def vars(self) -> S[Var]:
        return set([x for x in self.vals if isinstance(x, Var)])

    @property
    def sig(self) -> L[str]:
        '''Signature of tuple, giving the types of each arg'''
        return [x.dtype for x in self]

    @classmethod
    def fromlist(cls, xs: L[Any]) -> 'Tup':
        def f(x: Any) -> Val:
            if isinstance(x, str):
                return Const(x)
            elif isinstance(x, int):
                return Var(x)
            else:
                raise TypeError
        return cls([f(x) for x in xs])

    def substitute(self, var: Var, val: Val) -> None:
        '''Modify tuple, replacing a variable with a value'''
        if var in self:
            self.vals = tuple([val if v == var else v for v in self.vals])


class Rel(object):
    def __init__(self, name: str, attrs: L[str], tups: S[Tup]) -> None:
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

    @classmethod
    def empty(cls, name: str, attrs: L[str]) -> 'Rel':
        return cls(name, attrs, set())

    @classmethod
    def fromlist(cls, name: str, xs: L[L[Any]], attrs: L[str] = None) -> 'Rel':
        assert xs, "Gave empty relation to Rel.fromlist"
        tups = [Tup.fromlist(x) for x in xs]
        assert len(set([str(t.sig) for t in tups])) == 1
        attrs = attrs or list(ascii_lowercase[:len(xs[0])])
        assert len(attrs) == len(xs[0]), "Gave attr list with wrong #"
        return cls(name, attrs, set(tups))

    @property
    def max_var(self) -> int:
        return max([max([v.num for v in t.vars] or [0]) for t in self])

    def add(self, t: Tup) -> None:
        self.tups.add(t)

    def join(self, R: 'Rel', save_overlap: bool = False) -> 'Rel':
        overlap = set(self.attrs) & set(R.attrs)
        inds = [(self.attrs.index(x), R.attrs.index(x)) for x in overlap]
        selfinds = [i for i in range(len(self.attrs))
                    if save_overlap or self.attrs[i] not in overlap]
        Rinds = [i for i in range(len(R.attrs)) if R.attrs[i] not in overlap]
        res = Rel(self.name + ' ⋈ ' + R.name, [self.attrs[i] for i in selfinds
                                               ] + [R.attrs[i] for i in Rinds],
                  set())
        for tup in self:
            for Rtup in R:
                if all([tup[i] == Rtup[j] for i, j in inds]):
                    res.add(Tup([tup[i] for i in selfinds] + [
                        Rtup[i] for i in Rinds]))

        return res

    def substitute(self, var: Var, val: Val) -> None:
        '''Modify relation, replacing a variable with a value'''
        for t in self:
            t.substitute(var, val)


class Inst(object):
    def __init__(self, rels: L[Rel]) -> None:
        self.rels = {r.name: r for r in rels}

    def __iter__(self) -> I[Rel]:
        return iter(self.rels.values())

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
        return max([r.max_var for r in self])

    @classmethod
    def fromdict_noattr(cls, **kwargs: L[L[Any]]) -> 'Inst':
        return cls([Rel.fromlist(k, v) for k, v in kwargs.items()])

    @classmethod
    def fromdict(cls, **kwargs: T[L[str], L[L[Any]]]) -> 'Inst':
        return cls([Rel.fromlist(k, y, x) for k, (x, y) in kwargs.items()])

    @property
    def names(self) -> S[str]:
        return set(map(lambda r: r.name, self))


class Atom(object):
    def __init__(self, rel: str, tup: Tup) -> None:
        self.rel = rel
        self.tup = tup


class Query(metaclass=ABCMeta):

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
        self._var = atom.tup.vars if var is None else var
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

    def fire(self, i: Inst, counter: int = None) -> O[T[Inst, int]]:
        cntr = counter or i.max_var + 1
        return self._fire(i, cntr)

    @abstractmethod
    def _fire(self, i: Inst, counter: int) -> O[T[Inst, int]]:
        raise NotImplementedError


class EGD(Dependency):
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
        return set.union(*[x.vars for _, x in self.exists]) - self.q.var

    @property
    def bound_vars(self) -> S[Var]:
        return set.union(*[x.vars for _, x in self.exists]) & self.q.var

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
    def __init__(self, deps: S[Dependency]) -> None:
        self.deps = deps

    def chase(self, i: Inst) -> Inst:
        raise NotImplementedError


def main() -> None:
    pass


if __name__ == '__main__':
    main()
