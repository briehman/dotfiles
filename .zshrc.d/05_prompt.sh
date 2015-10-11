autoload -Uz promptinit && promptinit
autoload -Uz colors && colors
export PROMPT=$'%{$fg[cyan]%}%n%{$reset_color%}@%{$fg[red]%}%m%{$reset_color%} %{$fg[green]%}%d%{$reset_color%}\n%(#.#.$) '

