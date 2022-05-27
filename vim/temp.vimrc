" writing a new vimrc whilst learning vimscript the hard way

" how i like to see things
set number
set numberwidth=6
set nowrap
set sidescroll=8
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set ignorecase

" edit empty creates
augroup troi_global
	autocmd!
	autocmd BufNewFile * :write
augroup END

" my leader characters
let mapleader=","
let maplocalleader="\\"

" quickly edit and source .vimrc. this will not work with the $MYVIMRC
" variable in nvim since i actually use .vimrc, so the path is hard.
" coded.
:nnoremap <leader>ev :vsplit ~/.vimrc<cr>
:nnoremap <leader>sv :source ~/.vimrc<cr>

" quick open fold
nnoremap <leader><space> za

" move a line down or up. i'm not fond of these mappings.
nnoremap <leader>- ddp
nnoremap <leader>_ ddkkp

" uppercase current word. there is a bug if on first character of word.
" i haven't figured out how to use *, which correctly selects the word
" but does not seem to do what i need in visual mode.
inoremap <leader><c-u> <esc>bveU<esc>wi
nnoremap <leader><c-u> bveU<esc>w

" wrap current word in single or double quotes.
" TODO: this word selection is what i need to understand for uppercase
" word above.
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

" wrap visual selection in single or double quotes. losh suggested using
" `< and `> but i couldn't find a way to preserve the selection with the
" movement. this works for me.
vnoremap <leader>" xi""<esc>hp
vnoremap <leader>' xi''<esc>hp

" hardcore mode mappings to get out of the habit of using esc and arrow
" keys:
inoremap jk <esc>
inoremap <esc> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

" abbreviations for typos  and common
" text.
iabbrev @@ blametroi@gmail.com
iabbrev ccopy Copyright 2022 Troy Brumley, all rights reserved.
iabbrev ssig -- <cr>Troy Brumley<cr>blametroi@gmail.com

" filetype mappings
augroup filetype_basic
	autocmd!
	autocmd filetype basic nnoremap <buffer> <localleader>c I'<esc>
augroup END
augroup filetype_pascal
	autocmd!
	autocmd filetype pascal nnoremap <buffer> <localleader>c I//<esc>
augroup END

" testing autocommand groups
augroup filetype_html
	autocmd!
	autocmd FileType html nnoremap <buffer> <localleader>f Vatzf
augroup END
