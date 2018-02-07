# Revision history for jbi

## 0.2.0.0 -- 2017-09-xx

* Add a `--version` (and `-V`) flag to the executable.

* Try and support benchmarking with `cabal+nix` where possible.

    API change: the `NixSupport` type now contains information about
    `nix-instantiate`.

* If a build tool needs the version to check validity it has to
  explicitly obtain it.

    API Changes:

    * Change in `canUseCommand` in `BuildTool` class
    * Change in `command` field in `Valid` data structure
    * Change in `canUseMode` in `CabalMode` class
    * Addition of `needsMinCabal` to `CabalMode` class

* Other small tweaks to reduce the overhead of using _jbi_ over the
  build tool itself (parallel validity checking, etc.).

## 0.1.0.0 -- 2017-09-05

* First version. Released on an semi-suspecting world.
