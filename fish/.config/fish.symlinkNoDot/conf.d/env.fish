set -Ux DOTS $HOME/.dotfiles
set -Ux LANG en_US
set -Ux EDITOR vim

set -Ux DOCKER_HOST unix:///Users/$USER_HOSTNAME/.colima/docker.sock
set -Ux TESTCONTAINERS_RYUK_DISABLED true

set -Ux PYTORCH_TRACING_MODE TORCHFX
set -Ux COMMANDLINE_ARGS "--skip-torch-cuda-test --precision full --no-half"

set -Ux LDFLAGS "-L/usr/local/opt/openblas/lib"
set -Ux CPPFLAGS "-I/usr/local/opt/openblas/include"

if [ "$INSIDE_EMACS" ]
    set -Ux TERM "dumb"
end

. ~/.asdf/plugins/java/set-java-home.fish