{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "pure-prop";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "github.com/grumply/pure-prop";
  description = "A Prop class for building UI libraries.";
  license = stdenv.lib.licenses.bsd3;
}
