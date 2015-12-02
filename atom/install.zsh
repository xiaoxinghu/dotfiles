#!/usr/bin/env zsh

setopt EXTENDED_GLOB

home="${ZDOTDIR:-$HOME}/.atom"
if [[ -a "$home" ]]; then
  rm -rf $home
fi

ln -s "$(pwd -P)" "$home"
