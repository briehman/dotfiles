if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

GIT_PROMPT_FILE="$HOME/.bash_rc/git-prompt.sh"
if [[ -f "$GIT_PROMPT_FILE" ]]; then
  export GIT_PS1_SHOWDIRTYSTATE=true
  export GIT_PS1_SHOWCOLORHINTS=true
  export GIT_PS1_UNTRACKEDFILES=true
  . ~/.bash_profile.d/colors.sh
  export PROMPT_COMMAND="__git_ps1 '${txtgrn}\u@\h${txtrst}:${txtcyn}\w${txtrst}' '\\$ '"
  . $GIT_PROMPT_FILE
fi

export EDITOR=vim
export VISUAL=vim
add_path $HOME/bin
add_path $HOME/.rvm/bin
