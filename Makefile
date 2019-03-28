DIR = $(shell pwd)

.PHONY: all os

dots:
	ln -sfn $(DIR)/emacs.d ~/.emacs.d
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/xinitrc ~/.xinitrc
	ln -sfn $(DIR)/bash_aliases ~/.bash_aliases

os:
	sudo ln -sfn $(DIR)/os/kbd.rules /etc/udev/rules.d/kbd.rules
	sudo ln -sfn $(DIR)/os/kbd.sh /etc/udev/rules.d/kbd.sh
