{ mkDerivation, ArbitraryHaskell, base, QuickCheck, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "MLSpec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ ArbitraryHaskell base ];
  testDepends = [
    ArbitraryHaskell base QuickCheck tasty tasty-quickcheck
  ];
  homepage = "http://chriswarbo.net/git/mlspec";
  description = "Runs QuickSpec on sub-sets of Haskell definitions";
  license = stdenv.lib.licenses.publicDomain;
}
