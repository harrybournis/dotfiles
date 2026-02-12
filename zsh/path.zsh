export PATH="/usr/local/opt/rustup/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

if command -v mise &> /dev/null; then
    eval "$(mise activate zsh)"
else
    echo "Mise command not found. Make sure it is installed."
fi
