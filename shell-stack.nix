{ pkgs ? import ./nixpkgs.nix {}
, ghc ? pkgs.haskell.compiler.ghc8107
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "liquid-fixpoint-stack";

  buildInputs = [
    # z3 is necessary for tests and `stack exec fixpoint` to run.
    z3
  ];

  ghc = ghc;
}
