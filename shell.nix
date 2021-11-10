{pkgs ? import ./nixpkgs.nix {}}:

with pkgs;

mkShell {
  LANG="C.UTF-8";

  buildInputs = [
    haskell.compiler.ghc8107

    # z3 must be on the PATH so the `fixpoint` executable can find it.
    z3
  ];
}
