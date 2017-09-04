{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, Cabal, directory
      , filepath, optparse-applicative, process, stdenv, tagged, text
      }:
      mkDerivation {
        pname = "jbi";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base Cabal directory filepath process tagged
        ];
        executableHaskellDepends = [
          aeson-pretty base optparse-applicative text
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
