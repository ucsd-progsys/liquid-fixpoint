{ makeEnv ? false, config ? { }, ... }:
let
  # fetch pinned version of nixpkgs
  nixpkgs = import
    (
      builtins.fetchTarball {
        # fetch latest nixpkgs https://github.com/NixOS/nixpkgs-channels/tree/nixos-20.03 as of Tue 18 Aug 2020 02:51:27 PM UTC
        url = "https://github.com/NixOS/nixpkgs-channels/archive/cb1996818edf506c0d1665ab147c253c558a7426.tar.gz";
        sha256 = "0lb6idvg2aj61nblr41x0ixwbphih2iz8xxc05m69hgsn261hk3j";
      }
    )
    { inherit config; };
  # override haskell compiler version, add and override dependencies in nixpkgs
  haskellPackages = nixpkgs.haskell.packages."ghc8101".override (
    old: {

      all-cabal-hashes = nixpkgs.fetchurl {
        # fetch latest cabal hashes https://github.com/commercialhaskell/all-cabal-hashes/tree/hackage as of Wed 10 Mar 2021 05:25:16 PM UTC
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/bc95cd0e1029a0581e3df2f5cffc52867f6045b0.tar.gz";
        sha256 = "0k26992nabqpja1n43yky792jch9h3blb4pw5vivads2qd3l49vj";
      };
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        mkDerivation = args: super.mkDerivation (
          args // {
            doCheck = false;
            doHaddock = false;
            jailbreak = true;
          }
        );
        # test dependencies
        git = overrideCabal (self.callHackage "git" "0.3.0" { }) (
          old: {
            broken = false;
            # git-0.3.0 defines a Monad a fail function, which is incompatible with ghc-8.10.1 https://hackage.haskell.org/package/git-0.3.0/docs/src/Data.Git.Monad.html#line-240
            patches = [
              (nixpkgs.writeText "git-0.3.0_fix-monad-fail-for-ghc-8.10.1.patch" ''
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
          }
        );
        # build dependencies; using latest hackage releases as of Tue 18 Aug 2020 02:51:27 PM UTC
        memory = self.callHackage "memory" "0.15.0" { };
        z3 = overrideCabal super.z3 (old: { broken = false; });

        # this project
        liquid-fixpoint = nixpkgs.haskell.lib.overrideCabal (self.callCabal2nix "liquid-fixpoint" (nixpkgs.nix-gitignore.gitignoreSource [ ] ./.) { }) (old: {
          buildTools = [ nixpkgs.z3 ];
          doCheck = true;
          doHaddock = true;
          # bring the `fixpoint` binary into scope for tests run by nix-build
          preCheck = ''export PATH="$PWD/dist/build/fixpoint:$PATH"'';
        });

      };
    }
  );
  # function to bring devtools in to a package environment
  devtools = old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.cabal-install nixpkgs.ghcid ]; }; # ghc and hpack are automatically included
  drv = haskellPackages.liquid-fixpoint;
in
if makeEnv then drv.env.overrideAttrs devtools else drv
