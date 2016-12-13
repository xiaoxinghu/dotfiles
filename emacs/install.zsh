#!/usr/bin/env zsh

setopt EXTENDED_GLOB

if [[ ! -d ~/.emacs.d ]]; then
  git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

dotfiles=(spacemacs.d)
for dotfile in $dotfiles; do
  target="${ZDOTDIR:-$HOME}/.$dotfile"
  if [[ -h "$target" || -a "$target" ]]; then
    rm "$target"
  fi
  ln -s "$(pwd -P)"/$dotfile "$target"
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

# link snippets
for mode in "$(pwd -P)"/snippets/*; do
    if [[ -d $mode ]]; then
        target="${ZDOTDIR:-$HOME}/.emacs.d/private/snippets/${mode:t}"
        if [[ -h "$target" || -d "$target" ]]; then
            rm -rf $target
        fi
        ln -s "$mode" "$target"
    fi
done

# On Systemd Linux System
if [[ "$OSTYPE" != darwin* ]] && [[ $(pidof systemd) ]]; then
    userConfigDir="${ZDOTDIR:-$HOME}/.config/systemd/user"
    mkdir -p $userConfigDir
    for service in "$(pwd -P)"/*.service; do
        target="$userConfigDir/${service:t}"
        if [[ -h "$target" || -a "$target" ]]; then
            rm "$target"
        fi
        # link doesn't work for systemd, needs to be cp
        cp "$service" "$target"
    done
fi

