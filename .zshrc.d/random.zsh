#!/bin/zsh

# Press alt-q to push the current line on the buffer stack
bindkey '\eq' push-line-or-edit

# Probably do not want this on, but in case I think I do again, see below.
# Split on words (spaces) for use in for loops. You can get around this with
# ${=var}. See http://zsh.sourceforge.net/FAQ/zshfaq03.html for more detail.
#setopt shwordsplit
