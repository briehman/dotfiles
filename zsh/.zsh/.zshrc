# vim: ft=sh

# Environment variables {{{
export EDITOR=nvim
export VISUAL=nvim
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export LS_COLORS="$LS_COLORS:di=0;35:"

# Local {{{
# }}}
# }}}

# Aliases {{{
# Unix aliases
alias ag="ack-grep"
alias d=diff
alias gr=grep
alias grep='grep --color=auto'
alias j=jobs
alias v=nvim
alias vd=vimdiff
alias vi=nvim
alias gw=gradle_wrapper
alias mw=mvn_wrapper
alias c=bat
alias xo=xdg-open

# Shell aliases
alias f=fdfind
alias k=kill
alias ka=killall
alias ls="ls --color=auto"
alias ll="ls -Al --color=auto"
alias lld="ls -Ald --color=auto"
alias xg="xargs grep"
alias ..="cd .."

# Git aliases
alias g=git
alias ga="git add"
alias gs="git status"
alias gd="git diff"
alias gdw="git diff --word-diff"
alias gst="git staged"
alias gf="git fetch"
alias gg="git grep"
alias gl="git log"
alias glnm="git log --no-merges"
alias glsm="git log origin/master.."
alias glr="git log --reverse"
alias gp="git pull"

# Maven aliases
alias m=mvn
alias mc="mvn compile"
alias mcc="mvn clean compile"

# tmux aliases
alias tm=tmux
alias tma="tmux attach-session"
alias tml="tmux list-sessions"
alias ts=tmux-session

# Function
alias vgg="vimgitgrep"
alias vf="vimfind"

# vagrant
alias vg=vagrant

alias ff=firefox

# directories
alias dotf="cd ~/dotfiles"
alias org="cd ~/org"

alias md="python3 -m grip"

# Local {{{
# }}}
# }}}

# Functions {{{

vimfind() {
  nvim $(find "$@")
}

vimgitgrep() {
  nvim $(git grep -l "$@")
}

gcd() {
  local gitroot=$(git rev-parse --show-toplevel)

  if [[ ! -d "$gitroot" ]]; then
    echo "ERROR: Not in a Git directory. Use 'cd' instead." >&2
    return
  fi

  cd ${gitroot}/$1
}

search_jars() {
    needle="${1?Specify search argument}"
    shift
    for jar in "$@"; do
         jar tf $jar | grep -qs $needle && echo $jar
    done
}

gradle_wrapper() {
  if [ -f ./gradlew ]; then
    ./gradlew "$@"
  else
    $(git rev-parse --show-toplevel)/gradlew "$@"
  fi
}

mvn_wrapper() {
  $(git rev-parse --show-toplevel)/mvnw "$@"
}

llp() {
  for f in "$@"; do
    ls -al $PWD/$f
  done
}

# Local {{{
# }}}
# }}}

# Prompt {{{
autoload -Uz promptinit && promptinit
autoload -Uz colors && colors

export STARSHIP_CONFIG=$HOME/.starship
eval "$(starship init zsh)"
# }}}

# History settings {{{
# Ignore duplicates and spaces, share history between sessions
setopt inc_append_history hist_ignore_space histignorealldups sharehistory

# Keep N lines of history within the shell and save it to ~/.zsh_history:
HISTORY_IGNORE="(ls|cd|pwd|exit|cd ..)"
HISTSIZE=15000
SAVEHIST=15000
HISTFILE=~/.zsh_history
# }}}

# Vi mode {{{
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

# Let v edit the command
autoload edit-command-line && zle -N edit-command-line
bindkey -M vicmd v edit-command-line
# }}}

# Completion {{{
# Use modern completion system

ZCOMPDUMP_DIR=~/.cache/zsh/zcompdump-$ZSH_VERSION
set -e
[ ! -d $ZCOMPDUMP_DIR ] && mkdir -p $ZCOMPDUMP_DIR
autoload -Uz compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
set +e

zmodload -i zsh/complist

# man zshcontrib
zstyle ':vcs_info:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:*' enable git #svn cvs

# Enable completion caching, use rehash to clear
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Fallback to built in ls colors
zstyle ':completion:*' list-colors ''

# Make the list prompt friendly
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'

# Make the selection prompt friendly when there are a lot of choices
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Add simple colors to kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

zstyle ':completion:*' menu select=1 _complete _ignored _approximate

# insert all expansions for expand completer
# zstyle ':completion:*:expand:*' tag-order all-expansions

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

# ZAW styles
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10 # use 10 lines for filter-select
zstyle ':filter-select' max-lines -10 # use $LINES - 10 for filter-select
zstyle ':filter-select' rotate-list yes # enable rotation for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search no # see below

fpath=(${0:h}/zsh-completions/src $fpath)
# }}}

# Plugins {{{
#. $HOME/dotfiles/lib/z.sh
for plugin in $HOME/.zsh/plugins/**/*.plugin.zsh; do
  [[ -r "$plugin" && -f "$plugin" ]] && . $plugin
done
# }}}

# Functions {{{
ps1() {
    t="${1}"
    if [[ "$t" =~ "min" ]]; then
        starship_render() {}
        PS1="$ "
    elif [[ "$t" == "help" ]]; then
        echo "Allowed: minimal.  No args to restore";
    else
        . ~/.zshrc
    fi
}
# }}}

# Globbing {{{
# Allow unquoted globs - I know what I'm doing, zsh!
unsetopt nomatch
# }}}

# Random {{{
# Press alt-q to push the current line on the buffer stack
bindkey '\eq' push-line-or-edit

# Probably do not want this on, but in case I think I do again, see below.
# Split on words (spaces) for use in for loops. You can get around this with
# ${=var}. See http://zsh.sourceforge.net/FAQ/zshfaq03.html for more detail.
#setopt shwordsplit

# Store directory history and get tab-completion when entering "cd -<TAB>"
setopt AUTO_PUSHD
zstyle ':completion:*:directory-stack' list-colors '=(#b) #([0-9]#)*( *)==95=38;5;12'
# }}}

# Shell helpers {{{
# Local {{{
which rbenv >/dev/null 2>&1 && eval "$(rbenv init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
# }}}
# }}}
