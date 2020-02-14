{ mkDerivation, base, pure-core, template-haskell, stdenv }:
mkDerivation {
  pname = "pure-prop";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core template-haskell ];
  homepage = "github.com/grumply/pure-prop";
  description = "A Prop class for building UI libraries.";
  license = stdenv.lib.licenses.bsd3;
}
