#!/bin/bash
set -euxo pipefail

ROOT=$(realpath "$0" | xargs dirname)
ln -sf "$ROOT"/.emacs ~/.emacs
ln -sf "$ROOT"/.gitconfig ~/.gitconfig
mkdir -p ~/elisp && ln -sf "$ROOT"/elisp/* ~/elisp
