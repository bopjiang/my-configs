" !!!!!!! NOTICE WHEN FIRST TIME USE !!!!!!!!!!!!!!!!
" 1. download Vundle (https://github.com/gmarik/Vundle.vim)
"    git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" 2. run vim

set nocompatible              " be iMproved, required
filetype off                  " required


" Disable vim automatic visual mode on mouse select
set mouse-=a

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

"======================= My plugin =====================================

""" golang support
Plugin 'fatih/vim-go'



"======================= My plugin (end) ===============================

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" " To ignore plugin indent changes, instead use:
" "filetype plugin on
" "
" " Brief help
" " :PluginList       - lists configured plugins
" " :PluginInstall    - installs plugins; append `!` to update or just
" :PluginUpdate
" " :PluginSearch foo - searches for foo; append `!` to refresh local cache
" " :PluginClean      - confirms removal of unused plugins; append `!` to
" auto-approve removal
" "
" " see :h vundle for more details or wiki for FAQ
" " Put your non-Plugin stuff after this line

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim里面的编码主要跟三个参数有关：enc(encoding), fenc(fileencoding)和fencs(fileencodings)
" 其中fenc是当前文件的编码，也就是说，一个在vim里面已经正确显示了的文件(前提是你的系统环境跟你的enc设置匹配)，你可以通过改变 fenc后再w来将此文件存成不同的编码。比如说，我:set fenc=utf-8然后:w就把文件存成utf-8的了，:set fenc=gb18030再:w就把文件存成gb18030的了。这个值对于打开文件的时候是否能够正确地解码没有任何关系。
" fencs就是用来在打开文件的时候进行解码的猜测列表。文件编码没有百分百正确的判断方法，所以vim只能猜测文件编码。


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set fileencoding=utf-8 
set fileencodings=utf-8,gb18030,utf-16,big5

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Added on 22/12/2010 by Peter Jiang
""" ref: http://wangchunsheng.com/article-view-256.html
""" set expandtab        "使用space代替tab.a
set tabstop=4        "四个空格。 In PEP8 should be '8'
set softtabstop=4    " 
set autoindent
set shiftwidth=4     "自动缩进的宽度。
"""set textwidth=79     "一行的最大长度79
:syntax on
set nonumber

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Theme
"set background=dark
colorscheme elflord
"set Tlist_Inc_Winwidth=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if has("gui_running")
    set guioptions-=T
    set guioptions+=e
    set t_Co=256
    set guitablabel=%M\ %t
    set guifont=WenQuanYi\ Micro\ Hei\ Mono\ 10
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Added on 25/12/2010 by Peter Jiang
""" http://www.sontek.net/post/Python-with-a-modular-IDE-%28Vim%29.aspx
""" Ctags 
set tags+=$HOME/.vim/tags/python.ctags
"map <silent>;<;C-Left&gt; &lt;C-T&gt;      
"map &lt;silent&gt;&lt;C-Right&gt; &lt;C-]&gt;

""" Pydiction (Not Used, replaced byomnicompletion)
"let g:pydiction_location='~/.vim/pydiction/complete-dict'
"filetype plugin on 
"let g:pydiction_location='/home/jia/.vim/pydiction/complete-dict'

"""MiniBufExplorer (Not needed, Vim comes with in built-in tab support) 
"You don’t actually need MiniBufExplorer to get tabs in Vim.
"Vim 7 comes with tab support built right in! 
"Just :tabe myfile to edit a file in a new tab, 
"then :tabp and :tabn to switch between tabs.

"To turn on omnicompletion
"Which is built-in Vim 7
filetype plugin on
set ofu=syntaxcomplete#Complete


"Tasklist 
"is a simple plugin, too. Looking for the keyword of "TODO" in comment line
"
"Copying it into the plugin directory will suffice. 
"I like to have shortcuts and have added
map T :TaskList<CR>
map P :TlistToggle<CR>



""" Search Highlight 
set hlsearch


""" Complile graphviz and display the generated png file
"autocmd BufRead *.dot nmap <F8> :w<CR>:!dot -Tpng -o %<.png % && open %<.png<CR><CR>
autocmd BufRead *.dot nmap <F8> :w<CR>:!dot -Tpng -o %<.png % && eog %<.png<CR><CR>

set pastetoggle=<F9>

