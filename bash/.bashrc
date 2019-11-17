if [[ $0 == "zsh" ]]; then
        SCRIPT_DIR=${0:a:h}
else
        SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
fi

if [[ -z $SCRIPT_DIR ]]; then
        echo "SCRIPT_DIR empty!!"
        exit 1
fi

## import custom profile
source $SCRIPT_DIR/bash/default_bashrc.sh
source $SCRIPT_DIR/bash/custom_bashrc.sh

## editor
alias e='emacsclient -t'
alias ec='emacsclient -c'

## GNU tools
### replace system default awk and sed on mac
if [ "$(uname)" = "Darwin" ]; then
        alias awk=gawk
        alias sed=ased
        export PATH="/usr/local/opt/ncurses/bin:$PATH"
fi

## Golang
if [[ ! -z $GO_PKG_DIR ]]; then
        PATH=$GO_PKG_DIR/bin:$PATH
fi

if [[ ! -z $GOPATH ]]; then
        export PATH=$GOPATH/bin:$PATH
fi

## Python
export PATH="/usr/local/opt/python/libexec/bin:$PATH"


## Rust
export PATH=$HOME/.cargo/bin:$PATH

## tmux
source "${SCRIPT_DIR}/zsh/tmux.zsh"

## git
alias f='git difftool -d HEAD .'
alias gs="git status"
alias gd="git diff HEAD"

## other productive tools
export PATH="${SCRIPT_DIR}/bin:$PATH"

## other zsh profile
if [[ $0 == "zsh" ]]; then
        export HISTSIZE=9999
        export SAVEHIST=$HISTSIZE
        setopt hist_ignore_all_dups
        setopt hist_ignore_space
fi

