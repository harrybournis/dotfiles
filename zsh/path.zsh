export PATH="/usr/local/opt/rustup/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

# asdf
export PATH="$HOME/.asdf/shims:$PATH"
[[ -a $HOME/.asdf/plugins/java/set-java-home.zsh ]] &&
    source $HOME/.asdf/plugins/java/set-java-home.zsh
