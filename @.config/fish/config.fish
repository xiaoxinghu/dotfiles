# colors
set fish_color_command green

set -g -x PATH /usr/local/bin $PATH
set -g -x PATH /usr/local/sbin $PATH
set -g -x PATH ~/.emacs.d/bin $PATH
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
set -Ux EDITOR nvim

alias vim='nvim'

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

# vterm
function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/xiaoxing/Downloads/google-cloud-sdk/path.fish.inc' ]; . '/Users/xiaoxing/Downloads/google-cloud-sdk/path.fish.inc'; end

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.fish ]; and . ~/.config/tabtab/__tabtab.fish; or true
