# This Nix file provides an environment for running stack.
#
# Example usage:
#
#     nix-shell --pure --run "stack build"
#     nix-shell --pure --run "stack test"
#     nix-shell --pure --run "stack exec fixpoint -- tests/pos/adt.fq"

{pkgs ? import ./nixpkgs.nix {}}:

with pkgs;

mkShell {
  LANG="C.UTF-8";

  buildInputs = [
    haskell.compiler.ghc8107

    # nix is required because we use stack in "Nix mode", whereby stack invokes
    # `nix-shell` to create the reproducible build environment.
    nix

    stack
  ];
}
