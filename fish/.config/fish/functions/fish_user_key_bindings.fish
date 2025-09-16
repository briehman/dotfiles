function fish_user_key_bindings
  fish_vi_key_bindings

  # Use jk or jj as escape
  bind -M insert jk "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"
  bind -M insert jj "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"

  # Use ctrl-n and ctrl-p while in insert mode to move through history
  bind -M insert ctrl-p up-or-search
  bind -M insert ctrl-n down-or-search

  # Use ctrl-f to autocomplete the line
  bind -M insert ctrl-f forward-char
end
