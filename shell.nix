{pkgs ? import ./nixpkgs.nix {}}:

with pkgs;

mkShell {
  LANG="C.UTF-8";

  buildInputs = [
    haskell.compiler.ghc8107
    z3
  ];
}
