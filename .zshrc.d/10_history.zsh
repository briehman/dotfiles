# Ignore duplicates and spaces, share history between sessions
setopt inc_append_history hist_ignore_space histignorealldups sharehistory

# Keep N lines of history within the shell and save it to ~/.zsh_history:
HISTORY_IGNORE="(ls|cd|pwd|exit|cd ..)"
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
