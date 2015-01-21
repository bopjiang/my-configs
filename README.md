config of emacs and vim

## usage

### download
~~~bash
$ mkdir /tmp/my-config
$ git clone https://github.com/bopjiang/my-configs.git /tmp/my-config
$ cp -r /tmp/my-config/. ~/
~~~

### setup vim
  use bundle to manage all vim plugins
~~~bash
$ git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
$ vim +PluginInstall +qall
~~~

