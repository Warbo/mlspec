{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ArbitraryHaskell, base, bytestring
      , directory, hashable, haskell-src-exts, MissingH, nix-eval
      , process, QuickCheck, quickspec, stdenv, stringable, syb, tasty
      , tasty-quickcheck, template-haskell, temporary
      }:
      mkDerivation {
        pname = "mlspec";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson ArbitraryHaskell base bytestring hashable haskell-src-exts
          nix-eval QuickCheck quickspec stringable syb template-haskell
        ];
        executableHaskellDepends = [
          aeson ArbitraryHaskell base bytestring directory hashable
          haskell-src-exts MissingH nix-eval process QuickCheck quickspec
          stringable syb tasty tasty-quickcheck temporary
        ];
        testHaskellDepends = [
          aeson ArbitraryHaskell base bytestring directory hashable
          haskell-src-exts MissingH nix-eval process QuickCheck quickspec
          stringable syb tasty tasty-quickcheck temporary
        ];
        homepage = "http://chriswarbo.net/git/mlspec";
        description = "Runs QuickSpec on sub-sets of Haskell definitions";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
