#!/usr/bin/env zsh

setopt EXTENDED_GLOB

home="${ZDOTDIR:-$HOME}/.config/nvim"
vimrc="${ZDOTDIR:-$HOME}/.vimrc"
vimdir="${ZDOTDIR:-$HOME}/.vim"
if [[ -a "$home" ]]; then
  rm -rf $home
fi
if [[ -a "$vimdir" ]]; then
  rm -rf $vimdir
fi
if [[ -a "$vimrc" ]]; then
  rm $vimrc
fi

ln -s "$(pwd -P)/nvim" "$home"
ln -s "$(pwd -P)/nvim/init.vim" "$vimrc"
ln -s "$(pwd -P)/nvim" "$vimdir"
