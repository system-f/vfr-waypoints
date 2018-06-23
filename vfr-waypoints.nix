{ mkDerivation, base, checkers, hedgehog, lens, QuickCheck, stdenv
, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "vfr-waypoints";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens ];
  testHaskellDepends = [
    base checkers hedgehog lens QuickCheck tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/qfpl/vfr-waypoints";
  description = "VFR waypoints, as published in the AIP (ERSA)";
  license = stdenv.lib.licenses.bsd3;
}
