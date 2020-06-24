#!/bin/zsh

ps1() {
    t="${1}"
    if [[ "$t" =~ "min" ]]; then
        starship_render() {}
        PS1="$ "
    elif [[ "$t" == "help" ]]; then
        echo "Allowed: minimal.  No args to restore";
    else
        . ~/.zshrc.d/05_prompt.zsh
    fi
}
