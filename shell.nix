{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, Cabal, directory, filepath
      , process, stdenv, tagged
      }:
      mkDerivation {
        pname = "jbi";
        version = "0.1.0.0";
        src = /Users/ivan/Haskell/jbi;
        libraryHaskellDepends = [
          aeson base Cabal directory filepath process tagged
        ];
        description = "Just Build It - a \"do what I mean\" abstraction for Haskell build tools";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
