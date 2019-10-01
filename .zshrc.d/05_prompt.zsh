autoload -Uz promptinit && promptinit
autoload -Uz colors && colors

# These special unicode characters require a system font that supports them:
# https://powerline.readthedocs.org/en/latest/installation/linux.html#fonts-installation
PROMPT_HOST='%{%K{black}%F{white}%} %n %{%F{black}%K{blue}%}⮀ %{%F{white}%}%m %{%F{blue}%K{yellow}%}'
PROMPT_DIR='⮀%{%F{red}%K{yellow}%} %1(j.%j .)%F{black}%} %1~ '
PROMPT_SU='%(!.%{%k%F{black}%K{yellow}%}⮀%{%F{black}%} ⚡ %{%k%F{black}%}.%{%k%F{yellow}%})⮀%{%f%k%b%}'

PROMPT='%{%f%b%k%}
$PROMPT_HOST$PROMPT_DIR$PROMPT_SU '

