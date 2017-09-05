Just Build It, and hack on!
===========================

[![Hackage](https://img.shields.io/hackage/v/jbi.svg)](https://hackage.haskell.org/package/jbi) [![Build Status](https://travis-ci.org/ivan-m/jbi.svg)](https://travis-ci.org/ivan-m/jbi)

> A "do what I mean" abstraction for Haskell build tools.

Motivation
----------

You've decided to work on an existing Haskell project.  The repository
has been forked, you've cloned it to your computer, and you're about
to start work.  What's the first thing you need to do?

1) Replace all copyright notices with your own name.

2) Swap all tabs and spaces.

3) Convert all the code to Literate Haskell because it's such a pain
    to write your long prosaic comments whilst remembering to preface
    every line with `-- `.

Actually, unless you're someone with a religious obsession of using
what you prefer no matter what project you're working on or who you're
collaborating with, the first task you generally need to do is:

4) Work out which build tool is being used in the project.

After all, especially as we tend to put in more and more
metadata/hints into our different build tool files rather than just
using `runhaskell Setup.hs <foo>`, it's more convenient and friendlier
to work with a project the same way everyone else (especially the
maintainer!) does.

But this means you need to mentally switch gears and try and remember
the quirks of each individual tool's command line configuration (how
do I launch a REPL again?).  Your editing environment may need to be
configured so as to use the correct tool, whatever keyboard shortcuts
you use to run tests needs to change, etc.

Wouldn't it be nice if there was a simple way your development
environment (including your muscle memory!) could stay the same and
let some common interface handle the changing (without falling into
the trap of trying to [replace everything](https://xkcd.com/927/))?

Enter _jbi_
-----------

_jbi_ - short for "Just Build It" - is aimed at providing a common
interface between the various Haskell build tools.  You should be able
to enter any directory containing a Haskell project and just run `jbi`
and it will successfully determine the best build tool to use,
download dependencies and build the project.

Currently, _jbi_ knows of the following Haskell build tools:

* `stack` (with automatic [Nix] support)

* `cabal-install` with [Nix] support (using `cabal2nix` and `nix-shell`)

* `cabal-install` using sandboxes

[Nix]: https://nixos.org/nix/

Note that nothing within _jbi_ is inherently Haskell-oriented; it can
be extended to any build tool for any language which has similar
concepts for build tooling.

How _jbi_ works
---------------

To determine which build tool to works, _jbi_ takes into account three
things:

1. The order in which the tools are available to be checked in
   (currently the same as in the list above).

2. Whether a build tool is able to be used (i.e. the tool is installed
   and an appropriate project can be found).

3. Whether it is already being used.

Preference is given to tools already in evidence of being used.  As an
example, consider the following scenario:

```
myProjectDir/ $ ls
cabal.sandbox.config LICENSE myProject.cabal src/ stack.yaml
```

If both `cabal-install` and `stack` are available, then - despite the
presence of a `stack.yaml` - the presence of a sandbox configuration
indicates that a preference has been made for using them.

### Features

* Automatically install dependencies for and enable test-suites and
  benchmarks.

* Attempt to re-configure (including installing dependencies) if
  builds fail (which `stack` already provides)

* The equivalent of `cabal run` for `stack`.

* Print out a list of targets (equivalent of `stack ide targets`, for
  which `cabal-install` does not have an analogue).

* Detailed debugging information about tool availability.

* Work within any sub-directory of a project (no need to make sure
  you're running from the root directory!).

### Caveats

_jbi_ will not:

* Generate a `stack.yaml` for you.  This is an explicit opt-in of
  wanting to use `stack`, and furthermore isn't possible to determine
  whether you want it just for the current package or if it's part of
  a larger multi-package project.

* Install the result of the build for you.  _jbi_ is purely for
  developmental purposes.

* Allow you to not build the test suite or benchmarks (unless you
  specifically build a specific target).

* Allow you to have flexible builds, pass through extra command-line
  options, etc.  It is opinionated in how it does things to cover the
  common cases.

Furthermore:

* I have only recently started using [Nix] (both with Stack and
  cabal-install) and as such may not have it quite right (it seems to
  work with me though).

Fortuitously Anticipated Queries
--------------------------------

### Why isn't my build tool of choice being used?

Run `jbi info details` to find the information being used to choose
the build tool.  The chosen build tool will have:

* `"installation"` non-null.
* `"usable": true`
* A non-null `"project"`

Preference is given to:

* Build tools with `"artifactsPresent": true`
* Higher up in the list.

### What are these artifacts?

"Artifacts" is the term used by _jbi_ to denote the build-tool
specific files/directories found within the project that indicate it
is being worked upon.

These are:

_stack_
  ~ `.stack-work/`
_cabal+nix_
  ~ `shell.nix` or `default.nix`
_cabal+sandbox_
  ~ `cabal.sandbox.config` (note that the sandbox itself may be in a
      different directory)

`jbi prepare` will generate these; `jbi clean` will remove them (with
any other files/directories likely to have been produced as part of
the build process).  Typically you will never need to explicitly run
`jbi prepare`.

### Stack doesn't seem to be using Nix

For [Nix] support to work, you need to [configure your
  `stack.yaml`](https://docs.haskellstack.org/en/stable/nix_integration/).

### Why can't I use Stack with shell.nix?

For a project with no `.stack-work/`, _jbi_ takes the presence of a
`shell.nix` file to indicate that the project is using _cabal+nix_,
irregardless as to whether a `stack.yaml` file is present.

There are two ways you can work around this:

1. Explicitly create a `.stack-work/` directory; as _stack_ has a higher
   priority, _jbi_ will then pick it over _cabal+nix_.  Note, however,
   you may also need to explicitly run `stack setup` if using a
   non-system GHC.

2. Use a different filename other than `shell.nix` (remember to
   specify the filename properly in the `shell-file` section!).

The latter is preferred as it will allow more of _jbi_'s automatic
features to work (e.g. calling `stack setup`).

### How can I re-generate my shell.nix after updating my .cabal file?

**For _cabal+nix_.**

Run `jbi prepare`.  This is likely the only scenario you will ever
need to explicitly run this command in.

### How do I add a new build tool?

Pull requests are welcome.

To add a new tool, you need to create an instance of the `BuildTool`
class from `System.JBI.Commands.BuildTool`, and then insert your new
tool into an appropriate place in `defaultTools` in `System.JBI`.

### What about languages other than Haskell?

If, for some reason, you wish to use a language other than Haskell and
would like to use _jbi_ with it, you're more than welcome to send me a
pull request.
