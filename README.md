# dotfiles

These are my dotfiles, based on [holman's setup](https://github.com/holman/dotfiles).
They should be cloned in ~/.dotfiles. Running the script in script/bootstrap
will symlink all files with a .symlink extension in the home directory.

The macos folder contains scripts to set up a new mac machine with my default
settings and install all my packages and software. It also installs zsh, and [zim](https://zimfw.sh/).
Check out my [literate Emacs config.](https://github.com/harrybournis/dotfiles/blob/master/emacs.d.symlink/init.org)

**THE MACOS SCRIPT IS UNTESTED AND SHOULD NOT BE USED**

## Components

There are a few special files in the hierarchy.

- **bin/**: Anything in `bin/` will get added to your `$PATH` and be made
  available everywhere.
- **Brewfile**: This is a list of applications for [Homebrew Cask](https://caskroom.github.io) to install: things like Chrome and 1Password and Adium and stuff. Might want to edit this file before running any initial setup.
- **topic/install.sh**: Any file named `install.sh` is executed when you run `script/install`. To avoid being loaded automatically, its extension is `.sh`, not `.zsh`.
- **topic/\*.symlink**: Any file ending in `*.symlink` gets symlinked into
  your `$HOME`. This is so you can keep all of those versioned in your dotfiles
  but still keep those autoloaded files in your home directory. These get
  symlinked in when you run `script/bootstrap`.

## Symlink dotfiles

Existing files will be kept and renamed to `<filename>.backup`.
*Warning:* If it is run again the new symlinks may overwrite your backups, so
make sure that important files are safe.

```sh
git clone https://github.com/harrybournis/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./script/bootstrap
```

## Setup New Mac

Steps:
1. **Install zsh:** Run `~/.dotfiles/script/install-zsh.sh`
2. **Install brew and defaults:** Run `~/.dotfiles/macos/new-mac-setup.sh`
3. **Symlink dotfiles:** Run `~/.dotfiles/macos/bootstrap`

## Credits

This repo is inspired/copied from the following excelent dotfiles:

- [https://github.com/holman/dotfiles](https://github.com/holman/dotfiles) (fork and setup)
- [https://github.com/mathiasbynens/dotfiles](https://github.com/mathiasbynens/dotfiles) (macos defaults)
- [https://github.com/nicksp/dotfiles](https://github.com/nicksp/dotfiles) (zsh install script)
- [https://github.com/cowboy/dotfiles](https://github.com/cowboy/dotfiles) (macos install script)
- [https://github.com/paulirish/dotfiles](https://github.com/paulirish/dotfiles) (macos install script)
