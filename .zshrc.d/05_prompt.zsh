autoload -Uz promptinit && promptinit
autoload -Uz colors && colors

PROMPT_HOST='%{%K{black}%F{white}%} %n %{%F{black}%K{blue}%}⮀ %{%F{white}%}%m %{%F{blue}%K{yellow}%}'
PROMPT_DIR='⮀%{%F{black}%} %1~ '
PROMPT_SU='%(!.%{%k%F{blac}%K{yellow}%}⮀%{%F{black}%} ⚡ %{%k%F{black}%}.%{%k%F{yellow}%})⮀%{%f%k%b%}'

PROMPT='%{%f%b%k%}
$PROMPT_HOST$PROMPT_DIR$PROMPT_SU '

