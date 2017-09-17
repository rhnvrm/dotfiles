"vim-plug
call plug#begin('~/.vim/plugged')

Plug 'bling/vim-airline'

"Markdown Plugins
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'JamshedVesuna/vim-markdown-preview', {'for': 'markdown'}

"Vim notes
Plug 'xolox/vim-notes'
Plug 'xolox/vim-misc'

"NerdTree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

"Emmet
Plug 'mattn/emmet-vim'

call plug#end()

set nocompatible          " get rid of Vi compatibility mode. SET FIRST!
filetype plugin indent on " filetype detection[ON] plugin[ON] indent[ON]
set t_Co=256              " enable 256-color mode.
syntax enable             " enable syntax highlighting (previously syntax on).
set background=dark
colorscheme monokai       " set colorscheme
set number                " show line numbers
set laststatus=2          " last window always has a statusline
filetype indent on        " activates indenting for files
set nohlsearch            " Don't continue to highlight searched phrases.
set incsearch             " But do highlight as you type your search.
set ignorecase            " Make searches case-insensitive.
set ruler                 " Always show info along bottom.
set autoindent            " auto-indent
set tabstop=4             " tab spacing
set softtabstop=4         " unify
set shiftwidth=4          " indent/outdent by 4 columns
set shiftround            " always indent/outdent to the nearest tabstop
set expandtab             " use spaces instead of tabs
set smarttab              " use tabs at the start of a line, spaces elsewhere
"set nowrap                " don't wrap text
"set termguicolors

set wrap linebreak nolist

"KEY BINDINGS
"------------
"Alt+Arrow Navigation between splits"
nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

"Toggle Relative Numbering
nnoremap <F2> :set norelativenumber!<CR> 


"Airline-Status
set laststatus=2
let g:airline_powerline_fonts = 1

"Vim-Markdown and Markdown-Preview
let g:vim_markdown_folding_level = 3
let vim_markdown_preview_github=1

"NERDTree Commands 
"autocmd vimenter * NERDTree "Open NERDTree on start
"Toggle Nerdtree using <F9>
map <F9> :NERDTreeToggle<CR> 
"Change Default Symbols for NERDTree
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
"Change directory
let NERDTreeChDirMode=2

"For my life/dev logging purposes

"type nlog followed by space to start new log
iab <expr> nlog strftime("---\n\n%H:%M:%S")
