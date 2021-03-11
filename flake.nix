{

  description = "Liquid Fixpoint";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;
    #all-cabal-hashes = { flake = false; url = github:commercialhaskell/all-cabal-hashes/hackage; };
  };

  outputs = { self, nixpkgs }:
    let system = "x86_64-linux"; in
    {

      defaultPackage.${system} = (import nixpkgs {
        inherit system;
        overlays = self.overlays;
      }).haskellPackages.liquid-fixpoint;

      overlays = [

        # # build cabal2nix with a different package set as suggested by https://github.com/NixOS/nixpkgs/issues/83098#issuecomment-602132784
        # (final: prev: {
        #   cabal2nix-unwrapped = prev.haskell.lib.justStaticExecutables
        #     (prev.haskell.lib.generateOptparseApplicativeCompletion "cabal2nix" prev.haskell.packages."ghc882".cabal2nix);
        # })

        # fix haskell's git package
        (final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = selfH: superH: {
              git = prev.haskell.lib.overrideCabal (selfH.callHackage "git" "0.3.0" { }) (old: {
                broken = false;
                # git-0.3.0 defines a Monad a fail function, which is incompatible with ghc-8.10.1 https://hackage.haskell.org/package/git-0.3.0/docs/src/Data.Git.Monad.html#line-240
                patches = [
                  (prev.writeText "git-0.3.0_fix-monad-fail-for-ghc-8.10.1.patch" ''
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
            };
          };
        })

        # overlay to add our own package
        (final: prev: {
          haskellPackages = with prev.haskell.lib; prev.haskell.packages."ghc8101".override {
            # what happens if we use haskellPackages here?
            overrides = selfH: superH: {
              liquid-fixpoint = overrideCabal (prev.haskellPackages.callCabal2nix "liquid-fixpoint" self { }) (old: {
                buildTools = [ prev.z3 ];
                doCheck = true;
                doHaddock = true;
                # bring the `fixpoint` binary into scope for tests run by nix-build
                preCheck = ''export PATH="$PWD/dist/build/fixpoint:$PATH"'';
              });
            };
          };
        })

      ];

    };
}
