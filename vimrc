call plug#begin()
Plug 'nvim-treesitter/nvim-treesitter-context'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'itchyny/lightline.vim'
Plug 'neovim/nvim-lspconfig'
Plug 'windwp/nvim-autopairs'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'morhetz/gruvbox'
Plug 'vimwiki/vimwiki'
Plug 'matze/vim-move'
Plug 'axvr/org.vim'
"Plug 'simrat39/symbols-outline.nvim'
"Plug 'arcticicestudio/nord-vim'
"Plug 'mfussenegger/nvim-dap'
"Plug 'shaunsingh/nord.nvim'
"Plug 'szw/vim-maximizer'
"Plug 'junegunn/fzf.vim'
"Plug 'tpope/vim-commentary'
"Plug 'vifm/vifm.vim'
"Plug 'mbbill/undotree'
call plug#end()

set number
"set relativenumber
set fileformat=unix
set encoding=utf-8
set autoindent
set tabstop=2
set shiftwidth=2
set expandtab
set ruler
set noswapfile
set nobackup
set noundofile
set nohlsearch
set incsearch
set mouse=a
set novisualbell
set wildmode=full
set splitbelow splitright
set guicursor=
set noerrorbells
set scrolloff=2
set bg=dark
set cursorline
set signcolumn=yes
set wildcharm=<Tab>
set shell=/usr/bin/zsh
set showcmd
set foldlevel=92
set list
set listchars=tab:\¦\ ,trail:·,extends:…,precedes:…
set showbreak=↪
set wildmenu
set shortmess-=S
set shortmess+=at
set ignorecase
set completeopt=menu,menuone,noinsert
set path=**
set noshowmode
set sidescroll=2
set colorcolumn=100
set wildignorecase
set wildignore+=*.o,*.a,*.so,*.core,tags,Doxyfile
set laststatus=2
set viminfo-=f0
set omnifunc=syntaxComplete#Complete
set history=1000
set foldcolumn=2
"set nocompatible
"set foldmethod=syntax
"set cursorcolumn
"set complete=.

syntax on
filetype plugin on
command! -nargs=+ Silent
\   execute 'silent <args>'
\ | redraw!
filetype plugin on
packadd termdebug

nnoremap <C-w><C-w> :set nowrap!<CR>
nnoremap x "_dl
vnoremap x "_d
nnoremap S :%s///g<Left><Left><Left>
vnoremap s :s///g<Left><Left><Left>
nnoremap <C-w>n :tabnew<CR>
nnoremap <C-w>c :tabclose<CR>
inoremap <C-c> <ESC>
nnoremap <C-[> <ESC>
xnoremap <C-[> <ESC>
noremap <C-f> /
noremap <C-n> :
nnoremap <leader>o :e 
nnoremap <leader><C-o> :e **/*
nnoremap <leader>v :view 
nnoremap <leader><C-k> :view **/*
nnoremap <leader>r :read 
nnoremap <leader><C-v> :read */**
nnoremap <leader><C-r>f :Files<CR>
nnoremap <leader><C-r>g :Rg<CR>
nnoremap <leader><C-d>vt :vert new<CR>:vert term<CR>
noremap <leader><C-d>t :new<CR>:term<CR>
nnoremap <silent> <C-s> :if (hlstate == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=1-hlstate<cr>
nnoremap <leader>g :vimgrep // **<left><left><left><left>
nnoremap <leader><C-g> :vimgrep // %<left><left><left>
nnoremap g, :cp<CR>
nnoremap g. :cn<CR>
nnoremap <leader><C-d><C-w> :vert diffsplit **/*
nnoremap <leader><C-d>w :vert diffsplit 
nnoremap <leader>p :pwd<CR>
nnoremap <leader>C :cd **/*
nnoremap <leader>c :cd 
nnoremap <C-p> <C-y>
xnoremap <C-p> <C-y>
inoremap <C-f> <C-x><C-f>
nnoremap <leader><C-r>m :set ignorecase!<CR>
inoremap <expr> <CR> pumvisible() ? "\<C-Y>" : "\<CR>"
nnoremap <silent><leader><cr> :let searchTerm = '\v<'.expand("<cword>").'>' <bar> let @/ = searchTerm <bar> echo '/'.@/ <bar> call histadd("search", searchTerm) <bar> set hls<cr>
xnoremap <silent><leader><cr> yy:<C-u>let searchTerm = '\V'.substitute(escape(@", '\/'), "\n", '\\n', "g") <bar> let @/ = searchTerm <bar> call histadd("search", searchTerm) <bar> set hls <CR>
xnoremap <leader><C-d>g y:vimgrep /<C-r>"/ **
xnoremap <leader><C-d>f y/<C-r>"
nnoremap <leader>f :find *
nnoremap <leader>i :Explore 
nnoremap <Space>h <C-w>h
nnoremap <Space>j <C-w>j
nnoremap <Space>k <C-w>k
nnoremap <Space>l <C-w>l
nnoremap K s<CR><ESC>
nnoremap <C-t>h :-tabmove<CR>
nnoremap <C-t>l :+tabmove<CR>
nnoremap <C-t>s <C-w><C-t>
nnoremap <C-t><C-h> :tabprevious<CR>
nnoremap <C-t><C-l> :tabnext<CR>
nnoremap <leader><C-s>r :registers<CR>
nnoremap Y y$
nnoremap <leader><C-s>p :set paste!<CR>
nnoremap <esc>^[ <esc>^[
nnoremap <leader><C-s>b :buffers<CR>:buffer 
nnoremap <leader><C-s>o :sp #
nnoremap <leader><C-s>vo :vsp #
nnoremap <leader><C-s>m :marks<CR>
nnoremap <leader><C-s>j :jumps<CR>
nnoremap <leader><C-d>o :only!<CR>
nnoremap <Space>L :vert resize +5<CR>
nnoremap <Space>H :vert resize -5<CR>
nnoremap <Space>K :resize +5<CR>
nnoremap <Space>J :resize -5<CR>
nnoremap <leader><C-d>e :e!<CR>
nnoremap <leader><C-d>s :!
tnoremap <C-o> <C-\><C-n>
xnoremap <expr> p 'pgv"'.v:register.'y`>'
xnoremap <expr> P 'Pgv"'.v:register.'y`>'
nnoremap <leader><C-d>c :make 
inoremap <C-o> <C-x><C-o>
nnoremap <leader><C-d>n :new<CR>
nnoremap <leader><C-d>vn :vert new<CR>
nnoremap , @@
nnoremap <C-,> @:
xnoremap <C-,> @:
inoremap <C-l> <C-x><C-l>
inoremap <C-d> <C-x><C-d>
inoremap <C-j> <C-y>
nnoremap <leader>j :b *
cnoremap <C-h> <Left>
cnoremap <C-j> <Down>
cnoremap <C-k> <Up>
cnoremap <C-l> <Right>
cnoremap <C-s> <Del>
"inoremap <C-y> <C-m>
"nnoremap <leader><C-d>u :earlier 1f<CR>
"nnoremap <leader>W :set complete+=k**<CR>
"nnoremap <leader><C-j> gj
"nnoremap <leader><C-k> gk
"nnoremap <leader>0 g0
"nnoremap <leader>$ g$

let mapleader = "\\"
let hlstate=0
let &t_SI = "\e[5 q"
let &t_EI = "\e[1 q"
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:vimwiki_list = [{'path':'/home/mh/Wiki/', 'syntax': 'markdown', 'ext': 'md'}]
let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
let g:vimwiki_markdown_link_ext = 1
let g:move_key_modifier = 'C'
let g:termdebug_wide=1
"let updatetime=2000

hi SpecialKey ctermfg=240 guifg=#585858

au BufRead,BufNewFile *\.sd  set filetype=sdlang
au BufRead,BufNewFile *\.h   set filetype=c
au BufRead,BufNewFile *\.in  set filetype=c
au BufRead,BufNewFile *\.gpl set filetype=lisp
au FileType python,javascript,json,lisp call rainbow#load()
au BufRead,BufNewFile *\.md set spell | set complete+=kspell

augroup quickfix
  autocmd!
  autocmd QuickFixCmdPost [^l]* cwindow
  autocmd QuickFixCmdPost l* lwindow
augroup END


if !has('nvim')
  set ttymouse=xterm2
endif
