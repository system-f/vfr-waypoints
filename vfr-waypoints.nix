{ mkDerivation, base, containers, dimensional, fuzzy
, geodetic-types, lens, monoid-subclasses, optparse-applicative
, stdenv
}:
mkDerivation {
  pname = "vfr-waypoints";
  version = "0.1.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers dimensional fuzzy geodetic-types lens
    monoid-subclasses
  ];
  executableHaskellDepends = [
    base fuzzy lens optparse-applicative
  ];
  homepage = "https://github.com/qfpl/vfr-waypoints";
  description = "VFR waypoints, as published in the AIP (ERSA)";
  license = stdenv.lib.licenses.bsd3;
}
