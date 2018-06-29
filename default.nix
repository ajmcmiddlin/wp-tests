{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", profiling ? false }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    (if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler}).override {
          overrides = self: super: {
            tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
            mkDerivation = args:
              super.mkDerivation (args // { enableLibraryProfiling = profiling; });
          };
       };

  drv = haskellPackages.callPackage ./wp-test.nix {};

  devTools =
    [ pkgs.cabal-install
      pkgs.mariadb
      haskellPackages.stylish-haskell
      haskellPackages.apply-refact
      haskellPackages.hlint
      haskellPackages.hindent
      haskellPackages.hasktags
    ];

  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      devTools ++
      [ (haskellPackages.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
      ];
  });

in
  if pkgs.lib.inNixShell then shellDrv.env else drv
