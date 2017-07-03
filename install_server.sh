#!/bin/bash
# install script for a basic setup
# This includes:
# - zsh, zprezo
# - vim
# - git
# - fonts
# - ranger

# Choose the correct package manager

echo "Please input the name of your package manager...."

read pacman

case $pacman in
    "pacman")
        install_command="sudo pacman -S"
        ;;
    "yum")
        install_command="sudo yum install"
        ;;
    "apt-get")
        install_command="sudo apt-get install"
        ;;
esac

echo "Installing the necessary packages ...."

$install_command vim zsh stow ranger

cd ~

echo "Base finished, now installing all remaining ones ...."
git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

echo "Installing zshprezto done!"

echo "Stowing the .dotfiles ..."
stow ~/.dotfiles/vim
stow ~/.dotfiles/zprezto
stow ~/.dotfiles/zsh
stow ~/.dotfiles/ranger

echo "Installing the fonts ...."
$install_command ttf-dejavu

cd ~
git clone https://github.com/powerline/fonts.git
cd fonts
./install.sh
cd ..
rm -rf fonts

echo "Please try using zsh now, if everything is working correctly you may choose to use it as your default shell vi chsh -s /bin/zsh"
