{ mkDerivation, base, hedgehog, http-client-tls, http-types, lens
, servant, servant-client, stdenv, tasty, tasty-hedgehog, text
}:
mkDerivation {
  pname = "wp-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base hedgehog http-client-tls http-types lens servant
    servant-client tasty tasty-hedgehog text
  ];
  description = "Hedgehog state machine tests for WordPress";
  license = stdenv.lib.licenses.bsd3;
}
