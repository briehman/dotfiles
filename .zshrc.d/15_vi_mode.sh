# Use vi keybindings
bindkey -v

# map jj as the esc key for vim mode
bindkey "jj" vi-cmd-mode

# map s the esc key for vim mode
# bindkey "

# Make Ctrl-P and Ctrl-N search the way I would expect in history
autoload up-line-or-beginning-search
autoload down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
