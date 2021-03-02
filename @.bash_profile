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

# env
if [ -d $HOME/bin ]; then
    export PATH=$PATH:$HOME/bin
fi

# svn env values
export SVN_EDITOR='vim'
export EDITOR=vim

# ruby
#if [ -d $HOME/.rbenv/bin ]; then
#    export PATH="$HOME/.rbenv/bin:$PATH"
#fi
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export PATH="$HOME/.cargo/bin:$PATH"
