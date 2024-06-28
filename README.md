# Dotfiles

## Overview

This repo is a skeleton/template/vanilla version of my dotfiles. [gnu
stow](https://www.google.com/url?sa=t&source=web&rct=j&opi=89978449&url=https://www.gnu.org/software/stow/)
is used to install the dotfiles.

## Using this repo

First, fork this repo.

Then, add your dotfiles:

    $ git clone git@github.com:username/dotfiles.git .dotfiles
    $ cd .dotfiles
    $  # edit files
    $ git push origin master

Finally, to install your dotfiles onto a new system:

    $ cd $HOME
    $ git clone git@github.com:username/dotfiles.git .dotfiles
    $ gnu stow <package>  # <package> is one of the folders in the main
      directory. For example `stow git` or `stow vim`.
