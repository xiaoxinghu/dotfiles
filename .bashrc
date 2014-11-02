# .bashrc is the place to put stuff that applies only 
# to bash itself, such as alias and function definitions, 
# shell options, and prompt settings. (You could also put
# key bindings there, but for bash they normally go into 
# ~/.inputrc.)

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# PS1='[\u@\h \W]\$ '
PS1="\[\e[0;37m\][\W]\$ \[\e[0m\]"


export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# alias
alias grep='grep --color'
alias egrep='egrep --color'
alias fgrep='fgrep --color'

export LS_OPTIONS='--show-control-chars --color=auto'
alias ls='ls $LS_OPTIONS -hF'
alias ll='ls $LS_OPTIONS -lhF'
