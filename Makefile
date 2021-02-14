DIR = $(shell pwd)

.PHONY: all

all:
	mkdir -p ~/.emacs.d
	ln -sfn $(DIR)/emacs.d/init.el ~/.emacs.d/init.el
	ln -sfn $(DIR)/emacs.d/lisp ~/.emacs.d/lisp
	ln -sfn $(DIR)/emacs.d/straight ~/.emacs.d/straight
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/bashrc ~/.bashrc
