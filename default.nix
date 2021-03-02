{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, groups, indexed-traversable, lib }:
      mkDerivation {
        pname = "graded-functors";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base groups indexed-traversable ];
        homepage = "https://github.com/emilypi/graded-functors";
        description = "Graded functors";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
