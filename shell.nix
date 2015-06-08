with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hipspecifyer, QuickCheck, stdenv, tasty
             , tasty-quickcheck
             }:
             mkDerivation {
               pname = "MLSpec";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base hipspecifyer ];
               testDepends = [ base QuickCheck tasty tasty-quickcheck ];
               homepage = "http://chriswarbo.net/git/mlspec";
               description = "Runs QuickSpec on sub-sets of Haskell definitions";
               license = stdenv.lib.licenses.publicDomain;
             }) {};
in
  pkg.env
