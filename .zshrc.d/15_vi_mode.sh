# Use vi keybindings
bindkey -v

# map jj as the esc key for vim mode
bindkey "jj" vi-cmd-mode

# These are pretty much unnecessary with zaw
# Make Ctrl-P and Ctrl-N search the way I would expect in history
bindkey '^P' up-line-or-search
bindkey '^N' down-line-or-search

# Let jj break from search mode
bindkey -M isearch 'jj' accept-search
bindkey -M isearch 'jj' accept-search
