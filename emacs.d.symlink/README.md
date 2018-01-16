# Emacs

I found that customizing emacs through the init.el file can get really messy,
and using the UI is even worse. I was happy to find that I could use
org-mode and literate programming to organize and compile my config file.
After digging through people's dotfiles on GitHub, I settled on the setup used by
[Joe di Castro](https://github.com/joedicastro/dotfiles/tree/master/emacs/.emacs.d)
 and [Lars Tveito](https://github.com/larstvei/dot-emacs).

This config sets up an `after-save-hook`, which takes all (selected) code blocks
from `init.org` and combines them into an `init.el` file. Every time you save
your org file, your emacs configuration gets updated.
The only requirement is that the file needs to be compiled manually the first
time by running this:

``` sh
$ emacs ~/.dotfiles/emacs.d.symlink/init.org --batch --eval='(org-babel-tangle)'
```

You can also use the `M-x customize` the settings, which will be saved in
`custom.el`. My config does not depend on this file, and I am currently
gitignoring it, but I may include it eventually if I start using the GUI
for visual settings (colors, fonts etc).

## Google Calendar and Org Mode

To enable [org-gcal mode](https://github.com/myuhe/org-gcal.el), copy the `.api-keys-sample` file as `.api-keys` and
fill in your Google Calendar email, client id and client secret. The first time
the applications opens you will be prompted to login you browser and copy and
paste a key in Emacs. I set it up according to this [post](http://cestlaz.github.io/posts/using-emacs-26-gcal/).

``` sh
$ cp ~/.dotfiles/emacs.d.symlink/.api-keys-sample ~/.dotfiles/emacs.d.symlink/.api-keys
```

## Credits
These configs were important in the conception and execution of this setup:

- https://github.com/joedicastro/dotfiles/tree/master/emacs/.emacs.d
- https://github.com/larstvei/dot-emacs/blob/master/init.org
- https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org
