#!/usr/bin/env bash

set -evuo pipefail
IFS=$'\n\t'

cabal --version

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ];
then
  zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz \
       > $HOME/.cabal/packages/hackage.haskell.org/00-index.tar;
fi

# Need to make travis_retry available https://gist.github.com/letmaik/caa0f6cc4375cbfcc1ff26bd4530c2a3
travis_retry() {
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    # ! { } ignores set -e, see https://stackoverflow.com/a/4073372
    ! { "$@"; result=$?; }
    [ $result -eq 0 ] && break
    count=$(($count + 1))
    sleep 1
  done

  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }

  return $result
}

travis_retry cabal update -v

sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config

cabal install --only-dependencies --enable-tests --enable-benchmarks --dry -v > installplan.txt
sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt

# check whether current requested install-plan matches cached package-db snapshot
if diff -u $HOME/.cabsnap/installplan.txt installplan.txt;
then
  echo "cabal build-cache HIT";
  rm -rfv .ghc;
  cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
  cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;
else
  echo "cabal build-cache MISS";
  rm -rf $HOME/.cabsnap;
  mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
  cabal install --only-dependencies --enable-tests --enable-benchmarks;
fi

# snapshot package-db on cache miss
if [ ! -d $HOME/.cabsnap ];
then
   echo "snapshotting package-db to build-cache";
   mkdir $HOME/.cabsnap;
   cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
   cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;
fi
