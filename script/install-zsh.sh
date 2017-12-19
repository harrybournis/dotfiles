# Credit goes to https://github.com/nicksp/dotfiles/blob/a4ac252df7d73f7d63d327af2767c5622c2e1903/setup.sh#L271
install_zsh () {

    # Test to see if zshell is installed. If it is:
    # *Does not take into account if the default zsh in mac exists
    if [ -f /usr/local/bin/zsh -o -f /usr/bin/zsh ]; then
        # Install Oh My Zsh if it isn't already present
        if [[ ! -d ~/.oh-my-zsh/ ]]; then
            sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
        fi

        # Install Spaceship zsh theme
        if [[ -d ~/.oh-my-zsh/ ]]; then
            if [[ ! -f ~/.oh-my-zsh/custom/themes/spaceship.zsh-theme ]] ; then
                sh -c "$(curl -o - https://raw.githubusercontent.com/denysdovhan/spaceship-zsh-theme/master/install.zsh | zsh)"
            fi
        fi

        # Install zsh-suggestions
        if [[ -d ~/.oh-my-zsh/ ]]; then
            if [[ ! -d ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions ]] ; then
                sh -c "$(git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions)"
            fi
        fi

        # Set the default shell to zsh if it isn't currently set to zsh
        if [[ ! $(echo $SHELL) == $(which zsh) ]]; then
            chsh -s $(which zsh)
        fi
    else
        # If zsh isn't installed, get the platform of the current machine
        platform=$(uname);
        # If the platform is Linux, try an apt-get to install zsh and then recurse
        if [[ $platform == 'Linux' ]]; then
            if [[ -f /etc/redhat-release ]]; then
                sudo yum install zsh
                install_zsh
            fi
            if [[ -f /etc/debian_version ]]; then
                sudo apt-get install zsh
                install_zsh
            fi
            # If the platform is OS X, tell the user to install zsh :)
        elif [[ $platform == 'Darwin' ]]; then
            echo "We'll install zsh, then re-run this script!"
            brew install zsh
            exit
        fi
    fi
}

install_zsh
