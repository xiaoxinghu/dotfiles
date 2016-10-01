#!/usr/bin/env zsh

setopt EXTENDED_GLOB

for file in "$(pwd -P)"/LaunchAgents/*.plist; do
  target="${ZDOTDIR:-$HOME}/Library/LaunchAgents/${file:t}"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$file" "$target"
done

dict="$(pwd -P)"/spelling/LocalDictionary
target="${ZDOTDIR:-$HOME}/Library/Spelling/${dict:t}"
if [[ -h "$target" || -a "$target" ]]; then
  rm "$target"
fi
ln -s "$dict" "$target"

for config in "$(pwd -P)"/dotfiles/*; do
    target="${ZDOTDIR:-$HOME}/.${config:t}"
    if [[ -h "$target" || -a "$target" ]]; then
        rm -rf "$target"
    fi
    ln -s "$config" "$target"
done
