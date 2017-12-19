path=('$HOME/.rbenv/bin' $path)

# init according to man page
if (( $+commands[rbenv] ))
then
  eval "$(rbenv init -)"
fi
