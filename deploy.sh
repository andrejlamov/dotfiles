#!/bin/bash
set -euxo pipefail

ROOT=$(realpath "$0" | xargs dirname)
ln -sf "$ROOT"/.emacs ~/.emacs
ln -sf "$ROOT"/.bashrc ~/.bashrc
ln -sf "$ROOT"/.bash_profile ~/.bash_profile
ln -sf "$ROOT"/.gitconfig ~/.gitconfig
ln -sf "$ROOT"/.tmux.conf ~/.tmux.conf
mkdir -p ~/.emacs.d/straight/versions && ln -sf "$ROOT"/straight/versions/* ~/.emacs.d/straight/versions
ln -sfn "$ROOT"/elisp ~/elisp
