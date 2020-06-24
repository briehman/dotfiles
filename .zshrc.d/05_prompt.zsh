autoload -Uz promptinit && promptinit
autoload -Uz colors && colors

export STARSHIP_CONFIG=~/.starship
eval "$(starship init zsh)"
