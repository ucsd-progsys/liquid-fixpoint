{

  description = "Liquid Fixpoint";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      composeOverlays = funs: builtins.foldl' nixpkgs.lib.composeExtensions (self: super: { }) funs;
      haskellOverlay = compiler: final: prev: new:
        let new-overrides = new.overrides or (a: b: { }); in
        {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${compiler} = prev.haskell.packages.${compiler}.override
                (old: old // new // {
                  overrides = self: super: old.overrides self super // new-overrides self super;
                });
            };
          };
        };
      haskellPackagesOverlay = compiler: final: prev: cur-packages-overlay:
        haskellOverlay compiler final prev { overrides = cur-packages-overlay; };
      ghc = "ghc8107"; # Based on https://github.com/ucsd-progsys/liquid-fixpoint/blob/develop/stack.yaml#L3
      mkOutputs = system: {

        defaultPackage = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay.${system} ];
        }).haskell.packages.${ghc}.liquid-fixpoint;

        devShell = self.defaultPackage.${system}.env;

        overlay = composeOverlays [
          self.overlays.${system}.updateAllCabalHashes
          self.overlays.${system}.addRestRewrite
          self.overlays.${system}.patchHaskellGit
          self.overlays.${system}.addLiquidFixpoint
        ];

        overlays = {
          updateAllCabalHashes = final: prev:
            {
              all-cabal-hashes = final.fetchurl {
                # fetch latest cabal hashes https://github.com/commercialhaskell/all-cabal-hashes/commits/hackage as of Fri May 27 06:40:19 PM UTC 2022
                url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/91cbef8524376834839ea2814010a0258a06e37e.tar.gz";
                sha256 = "01h8cd2b1w4060dyyh4zz604gpjyzhvvc0mb1aj18b1z2bcgfakj";
              };
            };
          addRestRewrite = final: prev: haskellPackagesOverlay ghc final prev (selfH: superH:
            with prev.haskell.lib; {
              rest-rewrite = overrideCabal
                (selfH.callCabal2nix "rest-rewrite"
                  # rest-rewrite-0.2.0 on hackage defines `instance Hashable (Map a b)` but this has been upstreamed and so we build rest-rewrite-0.2.1 (which removed that instance) from github source
                  (final.fetchFromGitHub {
                    owner = "zgrannan";
                    repo = "rest";
                    rev = "9637b77823ef3ceb909510cad2508e828767f6fb";
                    sha256 = "15mxdd2ipy4zw5sf8vah7d5f3qzshy9qddlga0ip9pighcv2f19g";
                  })
                  { })
                (old: {
                  buildTools = [ prev.z3 ];
                  doCheck = false; # rest: graphs/fig4.dot: openFile: does not exist (No such file or directory)
                });
            });
          patchHaskellGit = final: prev: haskellPackagesOverlay ghc final prev (selfH: superH:
            with prev.haskell.lib; {
              # git has a MFP bug and hasn't been fixed yet Wed Oct  6 10:46:02 AM PDT 2021
              git = prev.haskell.lib.overrideCabal (selfH.callHackage "git" "0.3.0" { }) (old: {
                broken = false;
                # git-0.3.0 defines a Monad a fail function, which is incompatible with ghc-8.10.1 https://hackage.haskell.org/package/git-0.3.0/docs/src/Data.Git.Monad.html#line-240
                patches = [
                  (prev.writeText "git-0.3.0_fix-monad-fail-for-ghc-8.10.x.patch" ''
                    diff --git a/Data/Git/Monad.hs b/Data/Git/Monad.hs
                    index 480af9f..27c3b3e 100644
                    --- a/Data/Git/Monad.hs
                    +++ b/Data/Git/Monad.hs
                    @@ -130 +130 @@ instance Resolvable Git.RefName where
                    -class (Functor m, Applicative m, Monad m) => GitMonad m where
                    +class (Functor m, Applicative m, Monad m, MonadFail m) => GitMonad m where
                    @@ -242,0 +243 @@ instance Monad GitM where
                    +instance MonadFail GitM where
                    @@ -315,0 +317 @@ instance Monad CommitAccessM where
                    +instance MonadFail CommitAccessM where
                    @@ -476,0 +479 @@ instance Monad CommitM where
                    +instance MonadFail CommitM where
                  '')
                ];
              });
            });
          addLiquidFixpoint = final: prev: haskellPackagesOverlay ghc final prev (selfH: superH:
            let callCabal2nix = prev.haskell.packages.${ghc}.callCabal2nix; in
            with prev.haskell.lib; {
              liquid-fixpoint = overrideCabal (callCabal2nix "liquid-fixpoint" self { }) (old: {
                buildTools = [ prev.z3 ];
                # bring the `fixpoint` binary into scope for tests run by nix-build
                preCheck = ''export PATH="$PWD/dist/build/fixpoint:$PATH"'';
              });
            });
        };

      };
    in
    flake-utils.lib.eachDefaultSystem mkOutputs;
}
