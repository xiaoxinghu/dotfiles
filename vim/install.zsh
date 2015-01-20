#!/usr/bin/env zsh

setopt EXTENDED_GLOB

if [[ ! -d ~/.vim/bundle/Vundle.vim ]]; then
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

for rcfile in "$(pwd -P)"/^install.zsh(.N); do
  target="${ZDOTDIR:-$HOME}/.${rcfile:t}"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$rcfile" "$target"
done
