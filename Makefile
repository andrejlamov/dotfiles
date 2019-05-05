DIR = $(shell pwd)

.PHONY: all os x emacs-deps

dots:
	ln -sfn $(DIR)/emacs.d ~/.emacs.d
	ln -sfn $(DIR)/gitignore ~/.gitignore
	ln -sfn $(DIR)/gitconfig ~/.gitconfig
	ln -sfn $(DIR)/tmux.conf ~/.tmux.conf
	ln -sfn $(DIR)/bash_aliases ~/.bash_aliases
emacs-deps:
	cpan RPC::EPC::Service DBI DBD::Pg
os:
	sudo ln -sfn $(DIR)/os/kbd.rules /etc/udev/rules.d/kbd.rules
	sudo ln -sfn $(DIR)/os/kbd.sh /etc/udev/rules.d/kbd.sh
x:
	ln -sfn $(DIR)/xinitrc ~/.xinitrc
