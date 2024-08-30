alias reload! 'source ~/.config/fish/config.fish'

alias c 'clear'
alias lsa 'ls -a'
alias ls. 'ls -a | grep "^\."'
alias oepn 'open'
alias brwe "brew"
alias yt 'yt-dlp'
alias yt-audio "yt-dlp -x"
alias youtube-dl-mp3 "youtube-dl --extract-audio --audio-format mp3 "
alias ip_local "ipconfig getifaddr en0"
alias ip_ext "dig +short myip.opendns.com @resolver1.opendns.com"
alias vimrc "vim ~/.vimrc"
alias fishrc "vim ~/.config/fish/config.fish"
alias hosts "sudo vim /etc/hosts"
alias s "git status"
alias log "git log --oneline --decorate --color --graph"
alias rm_ds_store "find . -type f -name '*.DS_Store' -ls -delete"
alias emacs-tangle-init "emacs ~/.dotfiles/emacs.d.symlink/init.org --batch --eval '(org-babel-tangle)'"
alias fix-grey-file 'SetFile -c "" -t ""'
alias gradle './gradlew'
alias lazy "lazydocker"
alias vi "vim"
alias vimf "vim -o (fzf)"

# tmux
alias ta 'tmux attach -t'
alias tad 'tmux attach -d -t'
alias ts 'tmux new-session -s'
alias tl 'tmux list-sessions'
alias tksv 'tmux kill-server'
alias tkss 'tmux kill-session -t'

# ruby
alias railskill "cat tmp/pids/server.pid | xargs -n1 -J pid kill -9 pid"
alias rspec "bundle exec rspec"

# in docker
alias docker-stats "docker ps -q | xargs docker stats --no-stream"
alias dockerc "docker compose"

if [ $INSIDE_EMACS ]
    function emacsfindfile
        emacsclient -e "(find-file \"$PWD/$1:q\")"
    end

    alias e "emacsfindfile"
else
    alias e "emacsclient -t"
end
