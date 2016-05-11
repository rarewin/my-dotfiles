# Emacs key bind
bindkey -e

# history
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# auto complete
autoload -U compinit; compinit

# color
autoload -Uz colors
colors
