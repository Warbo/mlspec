{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ArbitraryHaskell, base, directory
      , hashable, MissingH, process, QuickCheck, stdenv, stringable
      , tasty, tasty-quickcheck, temporary
      }:
      mkDerivation {
        pname = "MLSpec";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ aeson ArbitraryHaskell base hashable stringable ];
        testDepends = [
          aeson ArbitraryHaskell base directory hashable MissingH process
          QuickCheck stringable tasty tasty-quickcheck temporary
        ];
        homepage = "http://chriswarbo.net/git/mlspec";
        description = "Runs QuickSpec on sub-sets of Haskell definitions";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
