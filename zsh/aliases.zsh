alias reload!='. ~/.zshrc'

alias c='clear'
alias ls='ls -G --color=auto'
alias lsa='ls -a'
alias oepn='open'
alias brwe="brew"
alias yt='yt-dlp -f "bestvideo[height<=?1080]+bestaudio/best"'
alias yt-audio="yt-dlp -x"
alias yt-m4a='yt-dlp -f "bestaudio[ext=m4a]"'
alias ip_local="ipconfig getifaddr en0"
alias ip_ext="dig +short myip.opendns.com @resolver1.opendns.com"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"
alias s="git status"
alias log="git log --oneline --decorate --color --graph"
alias rm_ds_store="find . -type f -name '*.DS_Store' -ls -delete"
alias emacs-tangle-init="emacs ~/.dotfiles/emacs.d.symlink/init.org --batch --eval='(org-babel-tangle)'"
alias fix-grey-file='SetFile -c "" -t ""'
alias lazy="lazydocker"

# ruby
alias railskill="cat tmp/pids/server.pid | xargs -n1 -J pid kill -9 pid"
alias rspec="bundle exec rspec"

# in docker
alias docker-stats="docker ps -q | xargs docker stats --no-stream"
alias dockerc="docker-compose"

if [ $INSIDE_EMACS ]; then
    function emacsfindfile(){
        emacsclient -e "(find-file \"${PWD}/${1:q}\")"
    }

    alias e="emacsfindfile"
else
    alias e="emacsclient -t"
fi

alias eg="emacsclient -t --eval '(progn (magit-status) (delete-other-windows))'"

noderepl() {
    FILE_CONTENTS="$(< $1 )"
    node -i -e "$FILE_CONTENTS"
}
