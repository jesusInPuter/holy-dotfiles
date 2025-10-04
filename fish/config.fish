if status is-interactive

    set fish_greeting

    # -------------------------------------------------------------------------
    # Environment Variables & PATH
    # -------------------------------------------------------------------------

    set -U fish_user_paths $fish_user_paths ~/.local/share/gem/ruby/3.4.0/bin
    fish_add_path ~/.config/emacs/bin/

    set -g fish_emoji_width 2


    # -------------------------------------------------------------------------
    # Key Bindings
    # -------------------------------------------------------------------------

    # Enable vi-style key bindings for modal command-line editing.
    fish_vi_key_bindings

    # Enable fzf key bindings (e.g., Ctrl-T, Ctrl-R, Alt-C).
    fzf_key_bindings


    # -------------------------------------------------------------------------
    #Tool Initializations
    # -------------------------------------------------------------------------

    # Initialize Zoxide for intelligent directory navigation.
    zoxide init fish | source

    # Initialize The Fuck for command-line typo correction.
    thefuck --alias | source

    # -------------------------------------------------------------------------
    # Aliases
    # -------------------------------------------------------------------------

    # Navigation and file listing using modern tools.
    alias ls lsd
    alias ll 'lsd -l'
    alias la 'lsd -la'
    alias .. 'cd ..'

    # File viewing.
    alias cat bat

    # Editors.
    alias e 'emacs -nw'
    alias v nvim
    alias clone 'git clone'


    # -------------------------------------------------------------------------
    # Functions
    # -------------------------------------------------------------------------

    # Creates a directory and immediately navigates into it.
    function mk
        mkdir -p -- "$argv[1]"
        and cd -- "$argv[1]"
    end

    # Overrides `cd` to use Zoxide, then lists the directory contents.
    function cd
        z $argv
        and lsd
    end

    # Interactive process killer using fzf.
    function fkill
        set -l pids (ps -ef | sed 1d | fzf -m --header="Press TAB to select multiple processes" | awk '{print $2}')

        if test -n "$pids"
            set -l pids_str (string join " " $pids)
            kill -9 $pids_str
            echo "Killed: $pids_str"
        else
            echo "No process selected."
        end
    end


    # -------------------------------------------------------------------------
    # Fish Syntax Highlighting Colors
    # -------------------------------------------------------------------------
    set -g fish_color_autosuggestion '555' 'brblack'
    set -g fish_color_cancel -r
    set -g fish_color_command --bold
    set -g fish_color_comment red
    set -g fish_color_cwd green
    set -g fish_color_cwd_root red
    set -g fish_color_end brmagenta
    set -g fish_color_error brred
    set -g fish_color_escape 'bryellow' '--bold'
    set -g fish_color_history_current --bold
    set -g fish_color_host normal
    set -g fish_color_match --background=brblue
    set -g fish_color_normal normal
    set -g fish_color_operator bryellow
    set -g fish_color_param cyan
    set -g fish_color_quote yellow
    set -g fish_color_redirection brblue
    set -g fish_color_search_match 'bryellow' '--background=brblack'
    set -g fish_color_selection 'white' '--bold' '--background=brblack'
    set -g fish_color_user brgreen
    set -g fish_color_valid_path --underline


    # -------------------------------------------------------------------------
    # : Prompt
    # -------------------------------------------------------------------------

    starship init fish | source

end

# Created by `pipx` on 2025-09-29 19:48:27
set PATH $PATH /home/jesus/.local/bin
