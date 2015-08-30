{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ArbitraryHaskell, base, bytestring
      , directory, hashable, haskell-src-exts, MissingH, process
      , QuickCheck, quickspec, stdenv, stringable, syb, tasty
      , tasty-quickcheck, temporary
      }:
      mkDerivation {
        pname = "MLSpec";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          aeson ArbitraryHaskell base bytestring directory hashable
          haskell-src-exts MissingH process QuickCheck quickspec stringable
          syb tasty tasty-quickcheck temporary
        ];
        testDepends = [
          aeson ArbitraryHaskell base bytestring directory hashable
          haskell-src-exts MissingH process QuickCheck quickspec stringable
          syb tasty tasty-quickcheck temporary
        ];
        homepage = "http://chriswarbo.net/git/mlspec";
        description = "Runs QuickSpec on sub-sets of Haskell definitions";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
