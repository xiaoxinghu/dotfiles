#!/usr/bin/env zsh

setopt EXTENDED_GLOB

for file in "$(pwd -P)"/^install.zsh(.N); do
  target="${ZDOTDIR:-$HOME}/Library/LaunchAgents/${file:t}"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$file" "$target"
done
