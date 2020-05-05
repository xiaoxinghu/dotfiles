set -g -x PATH /usr/local/bin $PATH
set -g -x fish_greeting ''
# turn off the fancy font
# set -g theme_powerline_fonts no
# use nerd font
set -g theme_nerd_fonts yes
set -x GPG_TTY (tty)
# set JAVA_HOME
# set -Ux JAVA_HOME (/usr/libexec/java_home)
# rbenv
# status --is-interactive; and source (rbenv init -|psub)
set -Ux EDITOR emacs
if test -d ~/Library/Android/sdk
    set -g -x ANDROID_HOME ~/Library/Android/sdk
    set -g -x PATH $ANDROID_HOME/platform-tools $PATH
    set -g -x PATH $ANDROID_HOME/build-tools/27.0.3 $PATH
end

# rust
set PATH $HOME/.cargo/bin $PATH

# asdf
set asdfp (brew --prefix asdf)
if test -d $asdfp
    source $asdfp/asdf.fish
end

# overwrite everything
set -g -x PATH $HOME/.scripts $PATH

# bootstrap fisherman
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# alias
alias mux tmuxinator

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

starship init fish | source

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/xiaoxing/Downloads/google-cloud-sdk/path.fish.inc' ]; . '/Users/xiaoxing/Downloads/google-cloud-sdk/path.fish.inc'; end
