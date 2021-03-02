if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif
" Plugins
call plug#begin()

" Color Scheme
Plug 'altercation/vim-colors-solarized'

" all mighty ctrlp
" Plug 'kien/ctrlp.vim'
" unite
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'

" lean & mean status/tabline for vim that's light as air
Plug 'bling/vim-airline'

" markdown
Plug 'plasticboy/vim-markdown'

" auto complete
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/neocomplete'

" vim surround
Plug 'tpope/vim-surround'

" tmux support
Plug 'christoomey/vim-tmux-navigator'

" git
Plug 'tpope/vim-fugitive'

" pencil for writting
Plug 'reedes/vim-pencil'

" marked
Plug 'itspriddle/vim-marked'

" Syntax checking
Plug 'scrooloose/syntastic'

" fuzzy search
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

" Map the leader key to SPACE
let mapleader="\<SPACE>"

" Coding
filetype on             " enable filetype detection
syntax on               " enbale syntax highlighting

set showcmd             " Show (partial) command in status line.
set showmatch           " Show matching brackets.
set showmode            " Show current mode.
set ruler               " Show the line and column numbers of the cursor.
set number              " Show the line numbers on the left side.
set formatoptions+=o    " Continue comment marker in new lines.
set textwidth=80         " Hard-wrap long lines as you type them.
set expandtab           " Insert spaces when TAB is pressed.
set tabstop=2           " Render TABs using this many spaces.
set shiftwidth=2        " Indentation amount for < and > commands.

set noerrorbells        " No beeps.
set modeline            " Enable modeline.
set esckeys             " Cursor keys in insert mode.
set linespace=0         " Set line-spacing to minimum.
set nojoinspaces        " Prevents inserting two spaces after punctuation on a join (J)

" More natural splits
set splitbelow          " Horizontal split below current.
set splitright          " Vertical split to right of current.

if !&scrolloff
  set scrolloff=3       " Show next 3 lines while scrolling.
endif
if !&sidescrolloff
  set sidescrolloff=5   " Show next 5 columns while side-scrolling.
endif

noremap <silent> <ScrollWheelDown> :call comfortable_motion#flick(40)<CR>
noremap <silent> <ScrollWheelUp>   :call comfortable_motion#flick(-40)<CR>

set display+=lastline
set nostartofline       " Do not jump to first character with page commands.

" Color
set background=dark            " use dark theme
" solarized options
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
let g:solarized_termcolors=256
colorscheme solarized

" Tell Vim which characters to show for expanded TABs,
" trailing whitespace, and end-of-lines. VERY useful!
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif
set list                " Show problematic characters.

" Also highlight all tabs and trailing whitespace characters.
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\|\t/

" Search
set hlsearch            " Highlight search results.
set ignorecase          " Make searching case insensitive
set smartcase           " ... unless the query has capital letters.
set incsearch           " Incremental search.
set gdefault            " Use 'g' flag by default with :s/foo/bar/.
set magic               " Use 'magic' patterns (extended regular expressions).

" Use <C-L> to clear the highlighting of :set hlsearch.
nnoremap <silent> <leader>l :nohlsearch<CR><C-L>
" if maparg('<C-L>', 'n') ==# ''
"   nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
" endif

" Relative numbering
function! NumberToggle()
  if(&relativenumber == 1)
    set nornu
    set number
  else
    set rnu
  endif
endfunc

" Toggle between normal and relative numbering.
nnoremap <leader>r :call NumberToggle()<cr>

" Use ; for commands.
nnoremap ; :
" Use Q to execute default register.
nnoremap Q @q

" CtrlP, prefer unite.vim now
" Open file menu
" nnoremap <Leader>p :CtrlP<CR>
" Open buffer menu
" nnoremap <Leader>b :CtrlPBuffer<CR>
" Open most recently used files
" nnoremap <Leader>r :CtrlPMRUFiles<CR>
" let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

" airline
let g:airline#extensions#tabline#enabled = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#right_alt_sep = '|'
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = '|'
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = '|'
set laststatus=2
"let g:airline_theme= 'serene'

" markdown
" use front matter
let g:vim_markdown_frontmatter=1
" disable folding
let g:vim_markdown_folding_disabled=1

" navigation
nnoremap <left>  : bprevious<CR>
nnoremap <right> : bnext<CR>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" unite.ivm
if executable('ag')
  " Use ag(the silver searcher)
  " https://github.com/ggreer/the_silver_searcher
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_rec_async_command = ['ag', '--follow', '--nocolor', '--nogroup', '--hidden', '-g', '']
  let g:unite_source_grep_default_opts =
  \ '-i --vimgrep --hidden --ignore ' .
  \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
endif

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#profile('default', 'context', {
    \   'start_insert' : 1
    \ })
call unite#custom#source('line',
    \ 'matchers', 'matcher_fuzzy')

nnoremap <leader>b : <C-u>Unite -buffer-name=files buffer<CR>
nnoremap <leader>r : <C-u>Unite -buffer-name=files file_mru<CR>
nnoremap <leader>p : <C-u>Unite -buffer-name=files file_rec/async:! file/new<CR>

" Quick grep from cwd
nnoremap <silent> <leader>g : <C-u>Unite -winwidth=150 grep:%::<CR>
nnoremap <silent> <leader>G : <C-u>Unite -buffer-name=search -auto-preview -no-quit -no-empty grep:.::<CR>

if has('nvim')
  " Use deoplete.
  let g:deoplete#enable_at_startup = 1
else
  " Use neocomplete.
  let g:neocomplete#enable_at_startup = 1
endif

" pencil
let g:pencil#autoformat = 0
augroup pencil
  autocmd!
  autocmd FileType markdown,mkd call pencil#init()
  autocmd FileType text         call pencil#init()
augroup END

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_ruby_checkers = ["rubocop"]
