#!/bin/bash
set -euxo pipefail

ROOT=$(realpath "$0" | xargs dirname)
ln -sf "$ROOT"/.bashrc ~/.bashrc
ln -sf "$ROOT"/.bash_profile ~/.bash_profile
ln -sf "$ROOT"/.gitconfig ~/.gitconfig
ln -sf "$ROOT"/.tmux.conf ~/.tmux.conf
ln -sfn "$ROOT"/emacs ~/.config/emacs
