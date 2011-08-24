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
	Reload=>true
    	)
   
export {
     -- Semigroup functions
     semigroup,
     beautifySemigroup
     }

needsPackage "Polyhedra";
needsPackage "Binomials";

isZero = v -> (
     -- testing if a vector (list) is zero
     for e in v do if not e==0 then return false else continue;
     true);

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

semigroupCone = I -> posHull semigroup I;

TEST ///
R = QQ[a..d];
I = ideal(a*d-b*c)
sg = semigroup I
needsPackage "FourTiTwo"
assert (I == toBinomial (toricMarkov (sg), R))
///

-- Local Variables:
-- fill-column: 72
-- End: