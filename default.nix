with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "purescript-halogen-bootstrap";
  buildInputs = [
    psc-package
    nodePackages.bower
    nodePackages.pulp
    nodejs
    # purescript
  ];
}
