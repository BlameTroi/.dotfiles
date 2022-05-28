"
" Troy's .vimrc Environment
"
" adding more customizations after working through the Losh "hardway" book
" and reviewing jmoyers' gist on setting up a c dev environment with
" vim and tmux.

" Identify platform 
silent function! OSX()
    return has('macunix')
endfunction
silent function! LINUX()
    return has('unix') && !has('macunix') && !has('win32unix')
endfunction
silent function! WINDOWS()
    return  (has('win32') || has('win64'))
endfunction

" Basics 
set nocompatible        " Must be first line
if !WINDOWS()
    set shell=/usr/bin/zsh
endif

" Windows Compatible 
" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if WINDOWS()
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

" some tweaks from jmoyers ...

" just because
set encoding=utf-8

" allow :e file autocomplete in subdirectories
set path+=**
set wildmenu

" don't ctrlp into these
let g:ctrlp_custom_ignore = '\.git\|node_modules\|\.cache'

" allow director/project specific vimrc ... this is potentially
" insecure but i work alone so ... no biggie.
set exrc
set secure

" Vim-Plug 
call plug#begin('~/.vim/plugged')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'blametroi/autotags'
Plug 'itchyny/lightline.vim'
Plug 'farmergreg/vim-lastplace'
Plug 'dracula/vim',{'as':'dracula'}
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'caglartoklu/fbc.vim'

call plug#end()

" Vim UI 

set nocursorline                " Don't highlight current line
set tabpagemax=10               " Only show 10 tabs
set noshowmode                  " Lightline displays the current mode

if has('cmdline_info')
    set ruler                   " Show the ruler
    "set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " A ruler on steroids
    set showcmd                 " Show partial commands in status line and
    " Selected characters/lines in visual mode
endif

if has('statusline')
    set laststatus=2
    " Broken down into easily includeable segments
    "set statusline=%<%f\                     " Filename
    "set statusline+=%w%h%m%r                 " Options
    "set statusline+=\ [%{&ff}/%Y]            " Filetype
    "set statusline+=\ [%{getcwd()}]          " Current dir
    "set statusline+=%=%-14.(%l,%c%V%)\ %p%%  " Right aligned file nav info
endif

" how i like to see things
set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
set numberwidth=6               " xedit legacy
set showmatch                   " Show matching brackets/parenthesis
set incsearch                   " Find as you type search
set hlsearch                    " Highlight search terms
set winminheight=0              " Windows can be 0 line high
set ignorecase                  " Case insensitive search
set smartcase                   " Case sensitive when uc present
set wildmenu                    " Show list instead of just completing
set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set scrolljump=1                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set foldenable                  " Auto fold code
set foldmethod=indent           " indent makes the most sense to me
set foldlevelstart=99           " open most folds when starting
set sidescroll=8                " chunks

" formatting defaults, things such as makefiles will need overrides
set nowrap                      " Do not wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of two spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every two columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)

" make mouse available in either vim or nvim
if !has('nvim')
    set ttymouse=xterm2
endif

if has('nvim')
    set mouse=a
endif

" Filetype customization 
syntax enable
filetype plugin on
filetype indent on

" color scheme
set termguicolors
colorscheme dracula

" comments in italics
highlight comment cterm=italic, gui=italic

" nvim hack, guicursor applies to terminal mode as well???
" this disables changing the cursor to a thin vertical bar
" when in insert mode
set guicursor=

" Key remaps 

" my preferred leaders...
let mapleader = ","
let maplocalleader="\\"

" from vimtips wiki, syntax highlighting group under cursor
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . "> trans<" . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" from https://dougblack.io/words/a-good-vimrc.html, toggle fold in normal
nnoremap <space> za

" quickly edit and source .vimrc. this will not work with the $MYVIMRC
" variable in nvim since i actually use .vimrc, so the path is hard.
" coded.
:nnoremap <leader>ev :vsplit ~/.vimrc<cr>
:nnoremap <leader>sv :source ~/.vimrc<cr>

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

" edit empty creates
augroup troi_global
	autocmd!
    autocmd FileType * set fo-=cro
	autocmd BufNewFile * :write
augroup END

" basic syntax and other support
augroup troi_basic
    autocmd!
    autocmd BufNewFile,BufRead *.bas set ft=basic
    autocmd BufNewFile,BufRead *.bi  set ft=basic
    autocmd BufNewFile,BufRead *.bm  set ft=basic
    autocmd BufNewFile,BufRead *.bas compiler fbc
augroup END

" key mappings for specific filetypes
augroup filetype_basic
	autocmd!
	autocmd filetype basic nnoremap <buffer> <localleader>c I'<esc>
augroup END

augroup filetype_pascal
	autocmd!
	autocmd filetype pascal nnoremap <buffer> <localleader>c I//<esc>
augroup END

augroup filetype_html
	autocmd!
	autocmd FileType html nnoremap <buffer> <localleader>f Vatzf
augroup END

