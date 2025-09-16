if status is-interactive
    # Variables {{{
    set -Ux BACKSTOP_DIR $HOME/dev/work/backstop
    set -Ux MANPAGER "sh -c 'col -bx | bat -l man -p'"

    set -Ux JIRA_URL https://jira.backstop.solutions
    set -Ux JIRA_DEFAULT_PROJECT FB
    # }}}

    # Path {{{
    source $HOME/.profile.d/10_aliases
    test -e $HOME/.profile.d/10_aliases.local && source $HOME/.profile.d/10_aliases.local
    fish_add_path /opt/homebrew/bin
    fish_add_path $HOME/bin
    fish_add_path $HOME/node_modules/.bin
    fish_add_path $HOME/dev/work/devtools/bin/
    # }}}

    # Tools {{{
    /opt/homebrew/bin/rbenv init - --no-rehash fish | source

    eval (/opt/homebrew/bin/brew shellenv)

    starship init fish | source

    set -Ux SDKMAN_DIR "$HOME/.sdkman"
    # }}}
end

# pnpm
set -gx PNPM_HOME "/Users/brian.riehman/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
