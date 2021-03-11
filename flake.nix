{

  description = "Liquid Fixpoint";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-20.09;
    #all-cabal-hashes = { flake = false; url = github:commercialhaskell/all-cabal-hashes/hackage; };
  };

  outputs = { self, nixpkgs }:
    let system = "x86_64-linux"; in
    {

      overlays = [

        # # build cabal2nix with a different package set as suggested by https://github.com/NixOS/nixpkgs/issues/83098#issuecomment-602132784
        # (self: super: {
        #   cabal2nix-unwrapped = super.haskell.lib.justStaticExecutables
        #     (super.haskell.lib.generateOptparseApplicativeCompletion "cabal2nix" super.haskell.packages."ghc882".cabal2nix);
        # })

        # fix haskell's git package
        (self: super: {
          haskellPackages = super.haskellPackages.override {
            overrides = selfH: superH: {
              git = super.haskell.lib.overrideCabal (selfH.callHackage "git" "0.3.0" { }) (old: {
                broken = false;
                # git-0.3.0 defines a Monad a fail function, which is incompatible with ghc-8.10.1 https://hackage.haskell.org/package/git-0.3.0/docs/src/Data.Git.Monad.html#line-240
                patches = [
                  (super.writeText "git-0.3.0_fix-monad-fail-for-ghc-8.10.1.patch" ''
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
        (import ./overlay.nix)

      ];

      defaultPackage.${system} = (import nixpkgs {
        inherit system;
        overlays = self.overlays;
      }).haskellPackages.liquid-fixpoint;

    };
}
