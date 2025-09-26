if status is-interactive
  # Silence the greeting
  set fish_greeting

  # Add Emacs binaries to the PATH
  fish_add_path ~/.config/emacs/bin/

  # Initialize Zoxide
  zoxide init fish | source

  # Use lsd for listings
  alias ls lsd
  alias emacs 'emacs -nw'
  alias ll 'lsd -l'
  alias la 'lsd -la'
  # Use bat instead of cat
  alias cat bat
  # Quick navigation
  alias .. 'cd ..'
  # Override cd with zoxide
  alias cd z

  # Use vi key bindings for the command line
  fish_vi_key_bindings
end
