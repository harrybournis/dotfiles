alias reload!='. ~/.zshrc'

alias c='clear'
alias lsa='ls -a'
alias lsh='ls -a | grep "^\."'
alias c='clear'
alias oepn='open'
alias brwe="brew"
alias sublime='open -a /Applications/"Sublime Text.app"'
alias youtube-dl-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias ip_local="ipconfig getifaddr en0"
alias ip_ext="dig +short myip.opendns.com @resolver1.opendns.com"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias hosts="sudo vim /etc/hosts"
alias gpo="git push origin"
alias s="git status"
alias log="git log --oneline --decorate --color --graph"
alias rm_ds_store="find . -type f -name '*.DS_Store' -ls -delete"
alias dot="cd ~/.dotfiles"
alias emacs-tangle-init="emacs ~/.dotfiles/emacs.d.symlink/init.org --batch --eval='(org-babel-tangle)'"
alias e="emacsclient -t"

# tmux
alias ta='tmux attach -t'
alias tad='tmux attach -d -t'
alias ts='tmux new-session -s'
alias tl='tmux list-sessions'
alias tksv='tmux kill-server'
alias tkss='tmux kill-session -t'

# ruby
alias railskill="cat tmp/pids/server.pid | xargs -n1 -J pid kill -9 pid"
alias rspec="bundle exec rspec"
