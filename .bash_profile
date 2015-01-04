# ~/.bash_profile is the place to put stuff that 
# applies to your whole session, such as programs
# that you want to start when you log in (but not
# graphical programs, they go into a different
# file), and environment variable definitions.

[[ -f ~/.bashrc ]] && . ~/.bashrc

export TERM=xterm-256color

if [[ `uname` == 'Linux' ]]; then
    echo 'linux is running'
elif [[ `uname` == 'Darwin' ]]; then
    # use gnu coreutils
    if brew list | grep coreutils > /dev/null ; then
	PATH="$(brew --prefix coreutils)/libexec/gnubin:$PATH"
    fi
    # bash completion
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
	. $(brew --prefix)/etc/bash_completion
    fi
    export JAVA_HOME=`/usr/libexec/java_home`
fi

# eval `gdircolors -b $HOME/.dir_colors`

if [[ -f $HOME/.bash_profile.`hostname` ]]; then
    source $HOME/.bash_profile.`hostname`
fi
# source $HOME/.bash_profile.`hostname`

# env
if [ -d $HOME/bin ]; then
    export PATH=$PATH:$HOME/bin
fi

# svn env values
export SVN_EDITOR='vim'
export EDITOR=vim

# android
export ANDROID_HOME=/usr/local/opt/android-sdk

# python
export WORKON_HOME=$HOME/.venv

#export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
#export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
#source /usr/local/bin/virtualenvwrapper.sh

# ruby
#if [ -d $HOME/.rbenv/bin ]; then
#    export PATH="$HOME/.rbenv/bin:$PATH"
#fi
#if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
