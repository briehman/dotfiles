# Trigger zaw by default with Ctrl-X ;

# Bind history to Ctrl-R
bindkey '^R' zaw-history

zstyle ':filter-select:highlight' matched fg=yellow,standout
#zstyle ':filter-select' max-lines 10 # use 10 lines for filter-select
zstyle ':filter-select' max-lines -15 # use $LINES - 10 for filter-select
zstyle ':filter-select' rotate-list yes # enable rotation for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search yes
