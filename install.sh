#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

if [ ! -d ~/.vim/bundle/Vundle.vim ]; then
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

########## Variables

olddir=~/dotfiles_old		# old dotfiles backup directory
# list of files/folders to symlink in homedir
# files="vimrc bash_profile tmux.conf ssh bashrc emacs.d"
dotfiles=(".vimrc" ".bashrc" ".bash_profile" ".inputrc")

##########

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ..."
mkdir -p $olddir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
echo "Moving any existing dotfiles from ~ to $olddir"
for dotfile in ${dotfiles[@]}; do
	filename=$(basename $dotfile)
	if [ -f ~/$filename ]; then
		mv ~/$filename ~/dotfiles_old/
	fi
	echo "Creating symlink to $dotfile in home directory."
	ln -s $(pwd)/$dotfile ~/$filename
done
