#!/usr/bin/env zsh

setopt EXTENDED_GLOB

for rcfile in "$(pwd -P)"/^install.zsh(.N); do
  target="${ZDOTDIR:-$HOME}/.${rcfile:t}"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$rcfile" "$target"
done
