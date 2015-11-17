#!/usr/bin/env zsh

setopt EXTENDED_GLOB

if [[ ! -d ~/.emacs.d ]]; then
  git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

for rcfile in "$(pwd -P)"/^install.zsh(.N); do
  target="${ZDOTDIR:-$HOME}/.${rcfile:t}"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$rcfile" "$target"
done

for layer in "$(pwd -P)"/layers/*; do
    if [[ -d $layer ]]; then
        target="${ZDOTDIR:-$HOME}/.emacs.d/private/${layer:t}"
        if [[ -h "$target" || -d "$target" ]]; then
            rm -rf $target
        fi
        ln -s "$layer" "$target"
    fi
done
