###
### THIS THING IS UNTESTED
##

# Copied from:
# https://github.com/cowboy/dotfiles/blob/master/init/20_osx_homebrew.sh
# https://github.com/paulirish/dotfiles/blob/master/setup-a-new-machine.sh

# Exit if it is not a mac
if [[ ! "$OSTYPE" =~ ^darwin ]] ; then
    exit
fi

### XCode Command Line Tools
#      thx https://github.com/alrra/dotfiles/blob/ff123ca9b9b/os/os_x/installs/install_xcode.sh
if ! xcode-select --print-path &> /dev/null; then

    # Prompt user to install the XCode Command Line Tools
    xcode-select --install &> /dev/null

    # Wait until the XCode Command Line Tools are installed
    until xcode-select --print-path &> /dev/null; do
        sleep 5
    done

    print_result $? 'Install XCode Command Line Tools'

    # Point the `xcode-select` developer directory to
    # the appropriate directory from within `Xcode.app`
    # https://github.com/alrra/dotfiles/issues/13

    sudo xcode-select -switch /Applications/Xcode.app/Contents/Developer
    print_result $? 'Make "xcode-select" developer directory point to Xcode'

    # Prompt user to agree to the terms of the Xcode license
    # https://github.com/alrra/dotfiles/issues/10

    sudo xcodebuild -license
    print_result $? 'Agree with the XCode Command Line Tools licence'
fi

# Install brew
if [[ ! "$(type -P brew)" ]]; then
  e_header "Installing Homebrew"
  true | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Exit if, for some reason, Homebrew is not installed.
[[ ! "$(type -P brew)" ]] && e_error "Homebrew failed to install." && exit

echo "Homebrew installed. Installing packages..."

# Install everything from the Brewfile
brew bundle

echo "Brew finished."

echo "Applying mac defaults..."

# Load the default mac preferences
./defaults.sh

echo "Defaults applied. Cleaning up..."

brew cleanup
brew cask cleanup

echo "Finished. Restart."
