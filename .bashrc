# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# PS1='[\u@\h \W]\$ '
PS1="\[\e[0;37m\][\W]\$ \[\e[0m\]"

# alias
alias grep='grep --color'
alias egrep='egrep --color'
alias fgrep='fgrep --color'

export LS_OPTIONS='--show-control-chars --color=auto'
alias ls='ls $LS_OPTIONS -hF'
alias ll='ls $LS_OPTIONS -lhF'

# added by travis gem
[ -f /Users/xiaoxing/.travis/travis.sh ] && source /Users/xiaoxing/.travis/travis.sh
[ -d /usr/local/opt/android-sdk ] && export ANDROID_HOME=/usr/local/opt/android-sdk
