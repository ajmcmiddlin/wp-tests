{ mkDerivation, base, bytestring, hedgehog, http-client, http-types
, lens, mysql, servant, servant-client, stdenv, tasty
, tasty-hedgehog, text
}:
mkDerivation {
  pname = "wp-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base servant servant-client ];
  testHaskellDepends = [
    base bytestring hedgehog http-client http-types lens mysql
    servant-client tasty tasty-hedgehog text
  ];
  description = "Hedgehog state machine tests for WordPress";
  license = stdenv.lib.licenses.bsd3;
}
