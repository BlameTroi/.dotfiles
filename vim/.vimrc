"
"" Troy's .vimrc Environment

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
" 

" Basics 
set nocompatible        " Must be first line
if !WINDOWS()
    set shell=/usr/bin/zsh
endif
" 

" Windows Compatible 
" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if WINDOWS()
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif
" 

"
" autotags setup ...
let g:autotags_ctags_global_include = "/usr/include/freebasic/*"
"let g:autotags_ctags_languages = "basic,pascal,vim,rexx,sh,go,fortran,lisp,emacslisp,c"
"let g:autotags_ctags_langmap = "basic:.bas.bi.bm.bb,pascal:.pas.pp.lpr,rexx:.rexx,sh:.sh.zsh,go:.go,fortran:.f.for.f90.f95"
" 
" Vim-Plug 
call plug#begin('~/.vim/plugged')

" files and search
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() }}
Plug 'mileszs/ack.vim'
Plug 'ctrlpvim/ctrlp.vim'

" tags
Plug 'ludovicchabant/vim-gutentags'
" Plug 'vim-scripts/taglist.vim'
" Plug 'vim-scripts/autotags' -- dated, my updates below
Plug 'blametroi/autotags'
Plug 'preservim/tagbar'

" UI
Plug 'itchyny/lightline.vim'
Plug 'farmergreg/vim-lastplace'
Plug 'junegunn/vim-peekaboo'

" themes
Plug 'altercation/vim-colors-solarized'

" git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" basic language support
Plug 'caglartoklu/fbc.vim'

" editor behavior helpers
Plug 'preserVIM/nerdtree'
Plug 'preserVIM/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
"Plug 'maxbrunsfeld/vim-yankstack'

" markdown / writing
" vimwiki suggests putting these last
" Plug 'junegunn/goyo.vim'
" Plug 'reedes/vim-pencil'
" plug 'ajorgensen/vim-markdown-toc'
" Plug 'vimwiki/vimwiki'

call plug#end()

" 

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

set backspace=indent,eol,start  " Backspace for dummies
set linespace=0                 " No extra spaces between rows
set number                      " Line numbers on
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

" mouse available in vim
if !has('nvim')
    set ttymouse=xterm2
endif

if has('nvim')
    set mouse=a
endif

" Formatting 
set nowrap                      " Do not wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of two spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every two columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
" 

" Filetype customization 
syntax enable
filetype plugin on
filetype indent on

au FileType py set autoindent
au FileType py set smartindent
au FileType py set textwidth=79
au FileType * set fo-=cro

" basic syntax and other support
autocmd BufNewFile,BufRead *.bas set ft=basic
autocmd BufNewFile,BufRead *.bi  set ft=basic
autocmd BufNewFile,BufRead *.bm  set ft=basic
autocmd BufNewFile,BufRead *.bas compiler fbc

" UI Stuff 

" color scheme
"let g:solarized_termcolors=256
set t_Co=256
syntax enable
"colorscheme snazzy
"let g:SnazzyTransparent = 1
"colors blue
"colors qbcolor
"colors default
set background=dark
colorscheme solarized

" comments in italics
highlight comment cterm=italic, gui=italic

" nvim hack, guicursor applies to terminal mode as well???
" this disables changing the cursor to a thin vertical bar
" when in insert mode
set guicursor=

" 

" Plugin additions 

" Putting these after just about everything else is done
" Recommendations seem to be do them close to last

" qb64dev configuration
let g:qb64dev_qb64_directory="E:\\code\\qb64"
let g:qb64dev_auotofind_qb64=0


" lightline configuration
"let g:lightline = {
"\ 'colorscheme': 'snazzy',
"\ }

set grepprg=rg\ --vimgrep\ --smart-case\ --follow

" make ack plugin use ag, but ... it seems there is
" an ag.vim plugin that i should look into
if executable('ag')
let g:ackprg = 'ag --vimgrep'
endif

" use # to toggle relative number on and off while in normal mode
" useful when a range is needed
"nnoremap <silent> # :set relativenumber!<cr>

" key remaps for qb64 mode
"au BufEnter,BufNew *.bas nnoremap <F5>  : call qb64dev#QB64CompileAndRun()<cr>
"au BufEnter,BufNew *.bi  nnoremap <F5>  : call qb64dev#QB64CompileAndRun()<cr>
"au BufEnter,BufNew *.bm  nnoremap <F5>  : call qb64dev#QB64CompileAndRun()<cr>
"au BufEnter,BufNew *.bas nnoremap <F11> : call qb64dev#QB64Compile()<cr> 
"au BufEnter,BufNew *.bi  nnoremap <F11> : call qb64dev#QB64Compile()<cr> 
"au BufEnter,BufNew *.bm  nnoremap <F11> : call qb64dev#QB64Compile()<cr> 

" Key remaps 

" from vimtips wiki, syntax highlighting group under cursor
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . "> trans<" . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" from https://dougblack.io/words/a-good-vimrc.html, toggle fold in normal
nnoremap <space> za

" and finally ...
let mapleader = ","
