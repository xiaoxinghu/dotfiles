#!/usr/bin/env zsh

setopt EXTENDED_GLOB

for folder in **/*(/); do
  cd $folder
  if [[ -s "install.zsh" ]]; then
    source "$(pwd -P)/install.zsh"
  fi
  cd -
done
