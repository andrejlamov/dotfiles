DIR = $(shell pwd)

.PHONY: all

all:
	ln -sfFn $(DIR)/emacs.d ~/.emacs.d
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/bashrc ~/.bashrc
