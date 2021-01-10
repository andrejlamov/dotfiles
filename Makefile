DIR = $(shell pwd)

.PHONY: all

all:
	ln -sfn $(DIR)/emacs.d ~/.emacs.d
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/bashrc ~/.bashrc
