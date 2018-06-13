{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  env = import ./env.nix {inherit nixpkgs compiler; };
  devTools =
    [ env.pkgs.cabal-install
      env.haskellPackages.stylish-haskell
      env.haskellPackages.apply-refact
      env.haskellPackages.hlint
      env.haskellPackages.hindent
      env.haskellPackages.hasktags
    ];

  drv = env.pkgs.haskell.lib.overrideCabal env.drv (drv: {
    buildDepends =
      (drv.buildDepends or []) ++
      devTools ++
      [ (env.haskellPackages.hoogleLocal {
          packages =
            (drv.libraryHaskellDepends or []) ++
            (drv.executableHaskellDepends or []) ++
            (drv.testHaskellDepends or []) ;
        })
      ];
  });
in
  if env.pkgs.lib.inNixShell then drv.env else env.drv
