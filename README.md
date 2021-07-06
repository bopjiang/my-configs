config of emacs and vim

## usage

### download reop
```bash
$ git clone https://github.com/bopjiang/my-configs.git ~/my-configs
```
## emacs
```bash
$ ln -s ~/my-configs/emacs.d ~/.emacs.d
```

## vim
```bash
# forget about the config, I use emacs now...
# ln -s ~/my-configs/.vimrc ~/.vimrc
$ ln -s ~/my-configs/.vimrc_simple ~/.vimrc
```

## tmux
```bash
$ ln -s ~/my-configs/tmux.conf ~/.tmux.conf
```

## bash/zsh

add
`source "$HOME/my-configs/bash/.bashrc"`
to .zshrc if you use *zsh*.

add *customized config* to *$HOME/my-configs/customized/custom_bashrc.sh*

```bash
chsh -s /bin/zsh
```
### oh-my-zsh config
```bash
PROMPT='%{$fg[green]%}%M%{$fg_bold[red]%}➜ %{$fg_bold[green]%}%p%{$fg[cyan]%}%c %{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%} % %{$reset_color%}'


ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
```

##  References

### Emacs(for 26.3)
* http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
* http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/
* http://tleyden.github.io/blog/2016/02/07/configure-emacs-as-a-go-editor-from-scratch-part-3/

* http://studygolang.com/articles/4925  !!! SAVE ME WHEN CONFIG GO AUTO COMPLETE
* https://github.com/cobblau/MyEmacs/blob/master/.emacs



