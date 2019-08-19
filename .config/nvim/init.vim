let mapleader = " "

" vim-plug (plugins)
" automate download
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" install plugins
call plug#begin('~/.local/share/nvim/plugged')
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/goyo.vim' " distraction free editing
Plug 'mattn/calendar-vim'
Plug 'vim-ctrlspace/vim-ctrlspace'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'chriskempson/base16-vim'
Plug 'airblade/vim-gitgutter'
Plug 'vimwiki/vimwiki'
Plug 'tbabej/taskwiki'
Plug 'farseer90718/vim-taskwarrior'
Plug 'powerman/vim-plugin-AnsiEsc'
Plug 'kien/ctrlp.vim' " Hit <C>p for a list of files/buffers.
call plug#end()


" basics
set nocompatible
set hidden
syntax on
filetype plugin on
set encoding=utf-8
set number relativenumber
" colorscheme mycontrast
let base16colorspace=256
colorscheme base16-tomorrow-night
set mouse=a

" undo unlimited
set undodir=~/.vim/undodir
set undofile

" allow to y and p to global buffer
set clipboard+=unnamedplus

" true colors (disabled for urxvt)
" set termguicolors

" auto completion
set wildmode=longest,list,full

" goyo plugin
map <leader>f :Goyo \| set linebreak<CR>

" nerdtree plugin
map <leader>t :NERDTreeToggle<CR>
" change Default Symbols for NERDTree
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
" change directory
let NERDTreeChDirMode=2

" split fix
set splitbelow splitright

" shortcuts for split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" vim-ctrlspace
let g:CtrlSpaceDefaultMappingKey = "<C-space> "

" vimwiki
let g:vimwiki_list = [{'path': '~/Documents/wiki',
                      \ 'syntax': 'markdown', 'ext': '.md',
                      \ 'path_html': '~/Documents/wiki/html'}]
let g:taskwiki_markup_syntax = "markdown"
let g:taskwiki_maplocalleader=",t"
