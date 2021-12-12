call plug#begin()

" Coc
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" VIM enhancements
Plug 'justinmk/vim-sneak'

" GUI enhancements
Plug 'frazrepo/vim-rainbow'
Plug 'itchyny/lightline.vim'
Plug 'andymass/vim-matchup'

" Syntactic language support
Plug 'cespare/vim-toml'
Plug 'stephpy/vim-yaml'
Plug 'rust-lang/rust.vim'
Plug 'neovimhaskell/haskell-vim'

call plug#end()

" Completion
" Better completion
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect
" Better display for messages
set cmdheight=2
" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" =============================================================================
" # Editor settings
" =============================================================================
filetype plugin indent on
set autoindent
set encoding=utf-8
set scrolloff=2
set noshowmode
set hidden
set nowrap
set nojoinspaces
map f <Plug>Sneak_s
map F <Plug>Sneak_S
let g:sneak#s_next = 1
" Always draw sign column. Prevent buffer moving when adding/deleting sign.
set signcolumn=yes

" Enable rainbow
let g:rainbow_active = 1

" Sane splits
set splitright
set splitbelow

" Permanent undo
set undodir=~/.vimdid
set undofile

" Use medium tabs
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab

" Display all white characters
set list

autocmd BufRead,BufNewFile *.c,*.h,*.cpp,*.hpp,*.hs set shiftwidth=2 softtabstop=2 tabstop=2

" Proper search
set incsearch
set ignorecase
set smartcase
set gdefault

set relativenumber " Relative line numbers
set number " Also show current absolute line
set colorcolumn=80 " and give me a colored column
set showcmd " Show (partial) command in status line.
set mouse=a " Enable mouse usage (all modes) in terminals

" Show those damn hidden characters
" Verbose: set listchars=nbsp:¬,eol:¶,extends:»,precedes:«,trail:•
set listchars=nbsp:¬,extends:»,precedes:«,trail:•

" Map C-c to Esc
inoremap <C-c> <Esc>

" Ctrl+h to stop searching
vnoremap <C-h> :nohlsearch<cr>
nnoremap <C-h> :nohlsearch<cr>

" Neat goto end/begin line keys
map H ^
map L $

" No arrow keys --- force yourself to use the home row
nnoremap <up> <nop>
nnoremap <down> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" Left and right can switch buffers
nnoremap <left> :bp<CR>
nnoremap <right> :bn<CR>

" Rust-specific
let g:rustfmt_autosave = 1
autocmd Filetype rust set colorcolumn=100

" Haskell-specific
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1
let g:haskell_backpack = 1
