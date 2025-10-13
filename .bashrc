export TERM=xterm-256color
export COLORTERM=truecolor
export PS1='$(pwd)$ '
export HOMEBREW_NO_AUTO_UPDATE=1
export COMPOSE_MENU=0
export PATH="/opt/homebrew/bin":"~/.local/bin":$PATH
alias tmux="tmux new-session -A -s main"
alias emacs="emacs -nw"
