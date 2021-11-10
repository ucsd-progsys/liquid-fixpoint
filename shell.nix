{pkgs ? import ./nixpkgs.nix {}}:

with pkgs;

mkShell {
  LANG="C.UTF-8";

  buildInputs = [
    ghc
    ghcid
  ];
}
