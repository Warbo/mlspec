{ mkDerivation, aeson, ArbitraryHaskell, base, bytestring
, directory, hashable, haskell-src-exts, MissingH, nix-eval
, process, QuickCheck, quickspec, stdenv, stringable, syb, tasty
, tasty-quickcheck, template-haskell, temporary
}:
mkDerivation {
  pname = "MLSpec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ArbitraryHaskell base bytestring hashable haskell-src-exts
    nix-eval QuickCheck quickspec stringable syb template-haskell
  ];
  executableHaskellDepends = [
    aeson ArbitraryHaskell bytestring directory hashable
    haskell-src-exts MissingH process QuickCheck quickspec stringable
    syb tasty tasty-quickcheck temporary
  ];
  testHaskellDepends = [
    aeson ArbitraryHaskell base bytestring directory hashable
    haskell-src-exts MissingH nix-eval process QuickCheck quickspec
    stringable syb tasty tasty-quickcheck temporary
  ];
  homepage = "http://chriswarbo.net/git/mlspec";
  description = "Runs QuickSpec on sub-sets of Haskell definitions";
  license = stdenv.lib.licenses.publicDomain;
}
