set nocompatible
filetype off


" =============== plugins ===============

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Keep Plugin commands between vundle#begin/end.
" a Git wrapper so awesome
Plugin 'tpope/vim-fugitive'

" Perform all your vim insert mode completions with Tab
Plugin 'ervandew/supertab'


Plugin 'kien/ctrlp.vim'
set wildignore+=*.o,*.zip
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_root_markers = ['.git', '.ideal', 'setup.py', 'pom.xml', 'README.md']

Plugin 'Shougo/unite.vim'

" lean & mean status/tabline for vim that's light as air
Plugin 'bling/vim-airline'

" Color Scheme
" Plugin 'altercation/vim-colors-solarized'
Plugin 'tomasr/molokai'

" markdown
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
" set default folding level
let g:vim_markdown_initial_foldlevel=2
Plugin 'itspriddle/vim-marked'

" coffeescript
Plugin 'kchmck/vim-coffee-script'

" ruby
Plugin 'vim-ruby/vim-ruby'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


" =============== basic ===============

set number         " enable line number
syntax on          " ebale syntax highlighting
set encoding=utf-8 " you know
set laststatus=2   " for airline to get the status

" =============== indent ===============

set sw=2
set ts=2
set expandtab " always uses spaces instead of tab characters

" =============== look and feel ===============

set background=dark            " use dark theme
colorscheme molokai
let g:solarized_termcolors=256 " colorscheme solarized
set guifont=Monaco:h12         " colorscheme solarized
set gcr=a:blinkon0             " Disable cursor blink
if has("gui_running")          "for gui
	set go=aAce "remove toolbar
"	set transp=8
"	set showtabline=2
"	set columns=140
"	set lines=40
endif

" =============== airline ===============

set ttimeoutlen=50
set guifont=Source\ Code\ Pro\ for\ Powerline:h12 " use patched font
"let g:airline_powerline_fonts = 1                 " use fancy powerline fonts

" =============== spelling ===============

autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.markdown setlocal spell
set complete+=kspell

" =============== Explore ===============

let g:netrw_liststyle=3 "set to tree view by default

" =============== Key Mapping ===============
nnoremap <left>  : bprevious<CR>
nnoremap <right> : bnext<CR>
nnoremap <up>    : Unite -start-insert -buffer-name=files buffer<CR>
nnoremap <down>    : Unite -start-insert -buffer-name=files file<CR>

