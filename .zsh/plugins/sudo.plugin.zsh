# ------------------------------------------------------------------------------
# Description
# -----------
#
# sudo will be inserted before the command
#
# ------------------------------------------------------------------------------
# Authors
# -------
#
# * Dongweiming <ciici123@gmail.com>
#
# ------------------------------------------------------------------------------

sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == sudo\ * ]]; then
        LBUFFER="${LBUFFER#sudo }"
    else
        LBUFFER="sudo $LBUFFER"
    fi
}

zle -N sudo-command-line

# Bind to ctrl-s
bindkey '^s' sudo-command-line
