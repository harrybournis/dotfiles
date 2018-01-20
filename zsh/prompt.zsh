# Speceship Symbols
SPACESHIP_CHAR_SYMBOL="λ "
SPACESHIP_RUBY_SYMBOL=" "
SPACESHIP_PYENV_SYMBOL=" "
SPACESHIP_PYENV_COLOR="green"

# Spaceship Vi-mode
# Show vi indicator only when on normal mode
SPACESHIP_VI_MODE_SHOW=true
SPACESHIP_VI_MODE_PREFIX=""
SPACESHIP_VI_MODE_SUFFIX=""
SPACESHIP_VI_MODE_INSERT=""
SPACESHIP_VI_MODE_NORMAL="[N] "

# Do not truncate directories when in a git repo
SPACESHIP_DIR_TRUNC_REPO=false

# Spaceship Prompt Order
SPACESHIP_PROMPT_ORDER=(
    # time          # Time stampts section
    user          # Username section
    host          # Hostname section
    dir           # Current directory section
    git           # Git section (git_branch + git_status)
    # hg            # Mercurial section (hg_branch  + hg_status)
    package       # Package version
    node          # Node.js section
    ruby          # Ruby section
    # elixir        # Elixir section
    # xcode         # Xcode section
    # swift         # Swift section
    # golang        # Go section
    # php           # PHP section
    # rust          # Rust section
    haskell       # Haskell Stack section
    # julia         # Julia section
    docker        # Docker section
    # aws           # Amazon Web Services section
    venv          # virtualenv section
    # conda         # conda virtualenv section
    # pyenv         # Pyenv section
    # dotnet        # .NET section
    # ember         # Ember.js section
    # kubecontext   # Kubectl context section
    exec_time     # Execution time
    line_sep      # Line break
    # battery       # Battery level and status
    vi_mode       # Vi-mode indicator
    jobs          # Backgound jobs indicator
    exit_code     # Exit code section
    char          # Prompt character
)
