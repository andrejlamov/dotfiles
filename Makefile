DIR = $(shell pwd)

.PHONY: all os emacs-deps

dots:
	mkdir -p ~/.local/bin
	ln -sfn $(DIR)/emacs.d ~/.emacs.d
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/bash_aliases ~/.bash_aliases
	ln -sfn $(DIR)/screenlayout ~/.screenlayout
	ln -sfn $(DIR)/xinitrc ~/.xinitrc
	ln -sfn $(DIR)/os/kbd-us-intl.sh ~/.local/bin/kbd-us-intl.sh

emacs-deps:
	cpan RPC::EPC::Service DBI DBD::Pg

os:
	sudo ln -sfn $(DIR)/os/kbd.rules /etc/udev/rules.d/kbd.rules
	sudo ln -sfn $(DIR)/os/kbd.sh /etc/udev/rules.d/kbd.sh
