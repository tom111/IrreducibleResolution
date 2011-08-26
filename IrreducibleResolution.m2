-- distributed under the GPLv2 or later.

newPackage(
	"IrreducibleResolution",
	Version => "0.1",
	Date => "August 2011",
	Authors => {{
		  Name => "Thomas Kahle",
		  Email => "kahle@mis.mpg.de",
		  HomePage => "https://github.com/tom111/IrreducibleResolution"}},
    	Headline => "Irreducible Resolutions over affine semigroup rings",
	Configuration => { },
    	DebuggingMode => true,
	PackageExports => {"Polyhedra"},
	Reload=>true
    	)

needsPackage "Polyhedra";
needsPackage "Binomials";
   
export {
     -- Semigroup functions
     semigroup,
     semigroupCone,
     beautifySemigroup,
     -- isPointed is is exported from the polyhedra package, we just
     -- give an implementation for the type Matrix.
     semigroupIsGraded,
     -- Ring related
     makeMultigradedRing
     }

-- check wether a semigroup is pointed by examing it's cone
isPointed Matrix := s -> isPointed posHull s

semigroupIsGraded = s -> (
     kern := transpose gens kernel s;
     apply (entries kern, row -> (
	       if not sum row == 0 then return false));
     true)

isZero = v -> (
     -- testing if a vector (list) is zero
     for e in v do if not e==0 then return false else continue;
     true)

isNonnegative = v -> (
     for e in v do if not e>=0 then return false else continue;
     true)

semigroup = I -> (
    -- compute the cone that contains a given semigroup 
    -- Input: A toric ideal 
    -- Output: A cone (from the polyhedra package)
    
    -- To save time it is assumed that all variables are regular mod I

    -- get the lattice using the Binomials package
    R := ring I;
    pchar := partialCharacter(I, cellVariables=> gens R);
    lattice := pchar#"L";
    
    (snf,P,Q) := smithNormalForm lattice;
    -- As a presentation of the semigroup, use the rows of P that
    -- correspond to zero rows of snf
    matrix keys select (
	 hashTable pack (2, mingle (entries P, entries snf)), 
	 isZero))

beautifySemigroup = sg -> (
     -- This function attempts to beautify semigroup presentations, that
     -- is find a representation with generators in NN^n.  This does not
     -- preserve the semigroup, but it preserves the syzygys among
     -- generators, i.e. the toric ideal.  The function performs row
     -- operations on integer matrices such that the entries become
     -- positive.
     if not semigroupIsGraded sg then error "Not implemented for ungraded semigroups";
     -- partition into non-negative and other generators
     sortedgens := partition (isNonnegative, entries sg);
     -- now add the nonnegative ones to the others to form nonnegative ones
     nngens := sortedgens#true;
     othergens := sortedgens#false;
     newgens := apply (othergens, g -> (
	       newg := g;
	       while not isNonnegative newg do (
		    -- find a negative coordinate
		    pos := 0;
		    for i to length g -1 do
			 if not g#i >= 0 then (
			      pos = i;
			      break);
		    applicable := select (nngens, v -> (v#pos > 0));
		    apphash := partition (v -> sum v, applicable);
     	       	    -- the following adds the vector with minimal overall weight
		    newg = newg + apphash#(min keys apphash)#0;
		    );
     	       newg));
     matrix (newgens | nngens))

-- Return the cone that contains the semigroup from an Ideal
semigroupCone = method(TypicalValue=>Cone)
semigroupCone Ideal := I -> posHull semigroup I;
semigroupCone Matrix := s -> posHull s;

makeMultigradedRing = I -> (
     -- Given a toric ideal in a polynomial ring, this will return the
     -- multigraded affine semigroup ring kk[x]/I with the multigrading
     -- installed.
     sg := beautifySemigroup semigroup I;
     R' := newRing (ring I, Degrees=>entries transpose sg);
     R'/sub(I,R'))


TEST ///
R = QQ[a..d];
I = ideal(a*d-b*c)
sg = semigroup I
needsPackage "FourTiTwo"
assert (isPointed sg)
assert (semigroupIsGraded sg)
assert (I == toBinomial (toricMarkov (sg), R))
assert (I == toBinomial (toricMarkov (beautifySemigroup sg), R))
///

-- Local Variables:
-- fill-column: 72
-- End: