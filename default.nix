{ mkDerivation, ArbitraryHaskell, base, directory, hashable
, MissingH, process, QuickCheck, stdenv, tasty, tasty-quickcheck
, temporary
}:
mkDerivation {
  pname = "MLSpec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ ArbitraryHaskell base hashable ];
  testDepends = [
    ArbitraryHaskell base directory hashable MissingH process
    QuickCheck tasty tasty-quickcheck temporary
  ];
  homepage = "http://chriswarbo.net/git/mlspec";
  description = "Runs QuickSpec on sub-sets of Haskell definitions";
  license = stdenv.lib.licenses.publicDomain;
}
