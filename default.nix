{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", profiling ? false }:

let
  inherit (nixpkgs) pkgs;

  # Get those sweet lens functions
  dmap = pkgs.fetchgit {
    url = "https://github.com/mokus0/dependent-map";
    rev = "40c481c48bbcd15c4183b89b925b560c07f0eb54";
    sha256  = "1gyfscgqyazk2nm5pmgvc972k0m86xhjyq0lnz63rngq82y2242s";
    fetchSubmodules = true;
  };

  haskellPackages =
    (if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler}).override {
          overrides = self: super: {
            tasty-hedgehog = self.callHackage "tasty-hedgehog" "0.2.0.0" {};
            dependent-map = self.callCabal2nix "dependent-map" dmap {};
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
