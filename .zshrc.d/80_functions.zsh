#!/bin/zsh

ps1() {
    t="${1}"
    if [[ "$t" =~ "min" ]]; then
        RPS1=""
        PS1="$ "
    elif [[ "$t" == "help" ]]; then
        echo "Allowed: minimal.  No args to restore";
    else
        . ~/.zshrc
    fi
}
