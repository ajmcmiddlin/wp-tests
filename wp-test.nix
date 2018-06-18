{ mkDerivation, aeson, base, bimap, bytestring, containers
, dependent-map, dependent-sum, dependent-sum-template, ghc-prim
, hedgehog, http-client, http-media, http-types, lens, mysql
, servant, servant-client, stdenv, tasty, tasty-hedgehog, text
, time, unordered-containers
}:
mkDerivation {
  pname = "wp-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bimap containers dependent-map dependent-sum
    dependent-sum-template ghc-prim http-media servant servant-client
    text time unordered-containers
  ];
  testHaskellDepends = [
    base bytestring containers dependent-map dependent-sum hedgehog
    http-client http-types lens mysql servant-client tasty
    tasty-hedgehog text
  ];
  description = "Hedgehog state machine tests for WordPress";
  license = stdenv.lib.licenses.bsd3;
}
