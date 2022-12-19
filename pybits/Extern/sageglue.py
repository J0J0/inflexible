# Python helper module to be called from haskell

# Note: python3's int type corresponds to haskell's Integer (not to Int)!

# For now, let's import everything from sage.
# TODO: Try to import only what we need, e.g. 'PolynomialRing' and whatever else we use.
#       The sage function 'import_statements' could be useful:
#           import_statements(PolynomialRing)
#       returns
#           from sage.rings.polynomial.polynomial_ring_constructor import PolynomialRing
from sage.all import *

from collections.abc import Iterable, Sequence

# Set sage verbosity, -1 = ERROR, see
# https://doc.sagemath.org/html/en/reference/misc/sage/misc/verbose.html .
#
# In case we ever want to /increase/ the verbosity level, we should make
# sure that trivial input cases don't cause spurious
#  "Warning: falling back to very slow toy implementation.",
# e.g. for polynomial rings in zero variables.
set_verbose(-1)


# type hints (for marshalled representation of polynomials)
Coeff     = tuple[int, int]  # (numerator, denominator)
Exponent  = int
Monomial  = Sequence[Exponent]
Poly      = dict[Monomial, Coeff]
IdealGens = Iterable[Poly]

# base class of sage polynomials and sage ideals (both for multivariate
# polynomial rings)
SagePoly  = sage.rings.polynomial.multi_polynomial.MPolynomial
SageIdeal = sage.rings.polynomial.multi_polynomial_ideal.MPolynomialIdeal

# Each Monomial in each Poly must have length 'nvar'.
def idealDimension(nvar: int, polys: IdealGens) -> int:
    R = PolynomialRing(QQ, 't', nvar)
    polys = [R(p) for p in polys]
    ideal = R.ideal(polys)
    return int(ideal.dimension())

# Check whether the projection of some ideal onto the coordinate
# 'var_id' is finite.
#
# Each Monomial in each ideal generator must have length 'nvar'.
def hasFiniteProjection(nvar: int, polys: IdealGens, var_id: int) -> bool:
    R = PolynomialRing(QQ, 't', nvar)
    elim_vars  = filter(lambda v: v != R.gen(var_id), R.gens())
    elim_ideal = eliminationIdeal(nvar, polys, elim_vars)
    return 0 not in elim_ideal.gens()

# Each Monomial in each ideal generator must have length 'nvar'.
# Each Monomial in 'variables' must have degree 1, i.e. its elements
# must be 0 except for a single 1; similarily, each SagePoly must be
# a single variable.
def eliminationIdeal(nvar: int, polys: IdealGens,
                     variables: Iterable[Monomial | SagePoly]) -> SageIdeal:
    R = PolynomialRing(QQ, 't', nvar)
    polys = [R(p) for p in polys]
    ideal = R.ideal(polys, coerce=False)
    elim_vars = [R(v) for v in variables]
    return ideal.elimination_ideal(elim_vars)


# type hints for marshalling back to haskell
VarNum     = int
Degree     = int
Py2HsMonom = tuple[Degree, Iterable[tuple[VarNum,Exponent]]]
Py2HsPoly  = Iterable[tuple[Py2HsMonom,Coeff]]

def sagepoly2hs(poly: SagePoly) -> Py2HsPoly:
    coeff_to_int_tuple = lambda c: tuple(map(int, c.as_integer_ratio()))
    p_items = poly.dict().items()
    return ( ((e.unweighted_degree(), e.sparse_iter()), coeff_to_int_tuple(c))
            for e,c in p_items )

# Each Monomial in each ideal generator must have length 'nvar'.
def eliminationIdealGens(nvar: int, polys: IdealGens, var_ids: Iterable[int]) -> Iterable[Py2HsPoly]:
    R = PolynomialRing(QQ, 't', nvar)
    elim_vars = map(R.gen, var_ids)
    gens = eliminationIdeal(nvar, polys, elim_vars).gens()
    return map(sagepoly2hs, gens)
