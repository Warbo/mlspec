{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, QuickCheck, stdenv, syb, tasty
      , tasty-quickcheck
      }:
      mkDerivation {
        pname = "MLSpec";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ base syb ];
        testDepends = [ base QuickCheck syb tasty tasty-quickcheck ];
        homepage = "http://chriswarbo.net/git/mlspec";
        description = "Runs QuickSpec on sub-sets of Haskell definitions";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
