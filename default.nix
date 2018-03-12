{ mkDerivation, base, qhull, stdenv, vector }:
mkDerivation {
  pname = "qhull-simple";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base vector ];
  librarySystemDepends = [ qhull ];
  testHaskellDepends = [ base vector ];
  homepage = "http://nonempty.org/software/haskell-qhull-simple";
  description = "Simple bindings to Qhull, a library for computing convex hulls";
  license = stdenv.lib.licenses.bsd3;
}
