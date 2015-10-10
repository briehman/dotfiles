# Use vi keybindings
bindkey -v

# map jj as the esc key for vim mode
bindkey "jj" vi-cmd-mode

# Make Ctrl-P and Ctrl-N search the way I would expect in history
bindkey '^P' history-incremental-search-backward
bindkey '^N' history-incremental-search-forward

# Let jj break from search mode
bindkey -M isearch 'jj' accept-search
bindkey -M isearch 'jj' accept-search
