self: super: {

  haskellPackages = with super.haskell.lib; super.haskell.packages."ghc8101".override {
    #all-cabal-hashes = super.fetchurl {
    #  # fetch latest cabal hashes https://github.com/commercialhaskell/all-cabal-hashes/tree/hackage as of Wed 10 Mar 2021 05:25:16 PM UTC
    #  url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/bc95cd0e1029a0581e3df2f5cffc52867f6045b0.tar.gz";
    #  sha256 = "0k26992nabqpja1n43yky792jch9h3blb4pw5vivads2qd3l49vj";
    #};
    overrides = selfH: superH: {
      mkDerivation = args: superH.mkDerivation (
        args // { doCheck = false; doHaddock = false; jailbreak = true; enableLibraryProfiling = false; }
      );
      # test dependencies
      #git = super.haskell.lib.overrideCabal (selfH.callHackage "git" "0.3.0" { }) (old: {
      #  broken = false;
      #  # git-0.3.0 defines a Monad a fail function, which is incompatible with ghc-8.10.1 https://hackage.haskell.org/package/git-0.3.0/docs/src/Data.Git.Monad.html#line-240
      #  patches = [
      #    (super.writeText "git-0.3.0_fix-monad-fail-for-ghc-8.10.1.patch" ''
      #      diff --git a/Data/Git/Monad.hs b/Data/Git/Monad.hs
      #      index 480af9f..27c3b3e 100644
      #      --- a/Data/Git/Monad.hs
      #      +++ b/Data/Git/Monad.hs
      #      @@ -130 +130 @@ instance Resolvable Git.RefName where
      #      -class (Functor m, Applicative m, Monad m) => GitMonad m where
      #      +class (Functor m, Applicative m, Monad m, MonadFail m) => GitMonad m where
      #      @@ -242,0 +243 @@ instance Monad GitM where
      #      +instance MonadFail GitM where
      #      @@ -315,0 +317 @@ instance Monad CommitAccessM where
      #      +instance MonadFail CommitAccessM where
      #      @@ -476,0 +479 @@ instance Monad CommitM where
      #      +instance MonadFail CommitM where
      #    '')
      #  ];
      #});
      # build dependencies; using latest hackage releases as of Tue 18 Aug 2020 02:51:27 PM UTC
      #memory = selfH.callHackage "memory" "0.15.0" { };
      #z3 = overrideCabal superH.z3 (old: { broken = false; });
      # this project
      liquid-fixpoint = overrideCabal (super.haskellPackages.callCabal2nix "liquid-fixpoint" (super.nix-gitignore.gitignoreSource [ ] ./.) { }) (old: {
        buildTools = [ super.z3 ];
        doCheck = true;
        doHaddock = true;
        # bring the `fixpoint` binary into scope for tests run by nix-build
        preCheck = ''export PATH="$PWD/dist/build/fixpoint:$PATH"'';
      });
    };
  };

}
