{ mkDerivation, base, criterion, stdenv }:
mkDerivation {
  pname = "newtype-existential";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base criterion ];
  license = stdenv.lib.licenses.bsd3;
}
