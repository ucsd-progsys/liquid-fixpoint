{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, array, ascii-progress, async
      , attoparsec, base, bifunctors, binary, boxes, bytestring, cereal
      , cmdargs, containers, deepseq, directory, dotgen, fgl
      , fgl-visualize, filemanip, filepath, ghc-prim, git, hashable
      , intern, located-base, mtl, nettools, ocaml, parallel, parallel-io
      , parsec, pretty, process, stdenv, stm, syb, tasty, tasty-ant-xml
      , tasty-hunit, tasty-rerun, text, text-format, time, transformers
      , unordered-containers, z3
      }:
      mkDerivation {
        pname = "liquid-fixpoint";
        version = "0.8.0.1";
        src = ./.;
        configureFlags = [ "-fbuild-external" ];
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-terminal array ascii-progress async attoparsec base bifunctors
          binary boxes bytestring cereal cmdargs containers deepseq directory
          dotgen fgl fgl-visualize filemanip filepath ghc-prim hashable
          intern located-base mtl parallel parallel-io parsec pretty process
          syb text text-format time transformers unordered-containers
        ];
        executableHaskellDepends = [ base ];
        executableSystemDepends = [ ocaml ];
        testHaskellDepends = [
          base containers directory filepath mtl process stm tasty
          tasty-ant-xml tasty-hunit tasty-rerun text transformers
        ];
        testSystemDepends = [ git nettools z3 ];
        doCheck = false;
        homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
        description = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
