{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, Cabal, directory, filepath
      , groom, optparse-applicative, process, stdenv, tagged
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
        executableHaskellDepends = [ base groom optparse-applicative ];
        description = "Just Build It - a \"do what I mean\" abstraction for Haskell build tools";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
