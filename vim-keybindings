nnoremap Y y$
nnoremap x "_dl
vnoremap x "_d
nnoremap S :%s///g<Left><Left><Left>
vnoremap s :s///g<Left><Left><Left>
nnoremap U :redo<CR>
nnoremap <C-s> /
nnoremap <C-r> ?
inoremap <C-t> <ESC>yl"_dlpi
nnoremap <C-x>tn :tabnew<CR>
nnoremap <C-x>tc :tabclose<CR>
nnoremap <C-x>th :tabprevious<CR>
nnoremap <C-x>tl :tabnext<CR>
nnoremap <C-x>tL :+tabmove<CR>
nnoremap <C-x>tH :-tabmove<CR>
nnoremap <C-x><C-s> :w<CR>
nnoremap <C-x><C-w> :w 
nnoremap <C-x><C-c> :qa
nnoremap <C-x>k :bd<CR>
nnoremap <C-x>0 :q<CR>
nnoremap <C-x>1 :only<CR>
nnoremap <C-x>2 <C-w><C-s>
nnoremap <C-x>3 <C-w><C-v>
nnoremap <C-x><C-o> <C-w><C-w>
nnoremap <C-x><C-f> :e 
nnoremap <leader>pf :e **/*
nnoremap <C-x><C-r> :view 
nnoremap <leader>pr :view **/*
nnoremap <C-x><C-i> :read 
nnoremap <leader>pi :read */**
nnoremap <silent> <M-h> :if (hlstate == 0) \| nohlsearch \| else \| set hlsearch \| endif \| let hlstate=1-hlstate<cr>
nnoremap <leader>pg :vimgrep // **<left><left><left><left>
nnoremap <leader>bg :vimgrep // %<left><left><left>
nnoremap <leader>bw :set nowrap!<CR>
nnoremap <C-x><C-q> :set ro!<CR>
nnoremap <M-!> :!
nnoremap <M-u><M-!> :read !
nnoremap <leader>m :set ft=
nnoremap <C-x><C-d> :Explore 
nnoremap <leader>pc :make 
nnoremap g, :cp<CR>
nnoremap g. :cn<CR>
nnoremap <leader>vd :vert diffsplit
nnoremap <M-s>c :set ignorecase!<CR>
nnoremap <silent> <leader>sw :let searchTerm = '\v<'.expand("<cword>").'>' <bar> let @/ = searchTerm <bar> echo '/'.@/ <bar> call histadd("search", searchTerm) <bar> set hls<cr>
xnoremap <silent> <leader>sr yy:<C-u>let searchTerm = '\V'.substitute(escape(@", '\/'), "\n", '\\n', "g") <bar> let @/ = searchTerm <bar> call histadd("search", searchTerm) <bar> set hls <CR>
nnoremap <C-x>b :buffers<CR>:buffer 
nnoremap <leader>sp :set paste!<CR>
nnoremap <leader>sm :marks<CR>
nnoremap <leader>sj :jumps<CR>
nnoremap <leader>sr :registers<CR>
xnoremap <expr> p 'pgv"'.v:register.'y`>'
xnoremap <expr> P 'Pgv"'.v:register.'y`>'
cnoremap <C-b> <Left>
cnoremap <C-n> <Down>
cnoremap <C-p> <Up>
cnoremap <C-f> <Right>
cnoremap <C-d> <Del>
cnoremap <C-y> <C-r>"
cnoremap <C-g> <ESC>
