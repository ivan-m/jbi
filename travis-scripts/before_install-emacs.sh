#!/usr/bin/env bash

set -evuo pipefail
IFS=$'\n\t'

git clone https://github.com/rejeep/evm.git $HOME/.evm

export PATH=$HOME/.evm/bin:$PATH

evm config path /tmp

evm install $EVM_EMACS --use --skip
