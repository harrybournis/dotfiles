if status is-interactive
    # Commands to run in interactive sessions can go here
end

if not string length --quiet $USER_HOSTNAME
    set_color red
    echo "USER_HOSTNAME ENV variable is not set. Stuff may not work correctly!"
    set_color normal
end

source "/Users/korkolis/.config/fish/fish_eat_emacs_integration"
fzf --fish | source
zoxide init fish | source

source ~/.local.fish
