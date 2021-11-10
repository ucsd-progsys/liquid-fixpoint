{ pkgs ? import ./nixpkgs.nix {}
, ghc ? pkgs.haskell.compiler.ghc8107
}:

with pkgs;

haskell.lib.buildStackProject ({
  name = "liquid-fixpoint-stack";
  buildInputs = [ hostname z3 ];
  ghc = ghc;
})
