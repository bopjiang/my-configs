SCRIPT_DIR=`dirname ${BASH_SOURCE[0]-$0}`

if [[ -z $SCRIPT_DIR ]]; then
        echo "SCRIPT_DIR empty!!"
        exit 1
fi

## import default profile
source $SCRIPT_DIR/default_bashrc.sh
## import custom profile
if [ -f "$SCRIPT_DIR/custom_bashrc.sh" ]; then
        source $SCRIPT_DIR/custom_bashrc.sh
fi

## PATH
export PATH=/usr/local/bin:$PATH

## LANG
export LC_MESSAGES=en_US.UTF-8
export LC_ALL=en_US.UTF-8

## editor
alias e='emacsclient -t'
alias ec='emacsclient -c'

## GNU tools
### replace system default awk and sed on mac
if [ "$(uname)" = "Darwin" ]; then
        alias awk=gawk
        alias sed=gsed
        export PATH="/usr/local/opt/ncurses/bin:$PATH"
fi

## Golang
if [[ ! -z $GO_PKG_DIR ]]; then
        PATH=$GO_PKG_DIR/bin:$PATH
fi

### if GOPATH contains multiple path, we do not put them in path
if [[ ! -z $GOPATH && $GOPATH != *[:]* ]]; then
        export PATH=$GOPATH/bin:$PATH
fi

## Python
export PATH="/usr/local/opt/python/libexec/bin:$PATH"


## Rust
export PATH=$HOME/.cargo/bin:$PATH

## tmux
source "${SCRIPT_DIR}/../zsh/tmux.zsh"

## git
alias f='git difftool -d HEAD .'
alias gs="git status"
alias gd="git diff HEAD"
alias cl="cloc --exclude-dir=vendor "
alias grep="egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}"

## other productive tools
export PATH=$(dirname ${SCRIPT_DIR})/bin:$PATH

## other zsh profile
if [[ $0 == "zsh" ]]; then
        export HISTSIZE=9999
        export SAVEHIST=$HISTSIZE
        setopt hist_ignore_all_dups
        # do not record command with space perfix
        setopt hist_ignore_space
        setopt hist_fcntl_lock 2>/dev/null
        setopt hist_reduce_blanks
fi
