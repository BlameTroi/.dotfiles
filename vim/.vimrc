"
" Troy's .vimrc Environment
"
" adding more customizations after working through the Losh "hardway" book
" and reviewing jmoyers' gist on setting up a c dev environment with
" vim and tmux.
"
" things are going well enough that i'm willing to switch to neovim since
" it's faster and i don't have any vim9 dependencies. however, i'm keeping
" to vimscript instead of lua for customization so i can always fall back
" to vim if needed.

set nocompatible        " Must be first

" Set shell on non window systems
if !(has('win32') || has('win64'))
  set shell=/usr/bin/zsh
endif

" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if (has('win32') || has('win64'))
  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

set encoding=utf-8

" Vim-Plug
call plug#begin('~/.vim/plugged')

" colors
" Plug 'blametroi/vim-amber'
" Plug 'blametroi/vim-dichromatic'
" Plug 'blametroi/reloaded.vim'
" Plug 'andreasvc/vim-256noir'

" statusline
Plug 'KaraMCC/vim-streamline'
let g:streamline_enable_devicons = 1
let g:streamline_minimal_ui = 0

" other filetypes
" Plug 'jorengarenar/COBOL.vim'
" let g:cobol_legacy_code = 0
" let g:cobol_folding = 1
" let g:cobol_autoupper = 1
" let g:cobol_indent_data_items = 2
" let g:cobol_indent_id_paras = 0
" let g:cobol_comp_mp_cobc = 1
" let g:cobol_format_free = 1

" even better vim behavior
Plug 'ctrlpvim/ctrlp.vim'
let g:ctrlp_custom_ignore = '\.git\|node_modules\|\.cache'

Plug 'farmergreg/vim-lastplace'

" git
" Plug 'airblade/vim-gitgutter'

" fuzzy and grep
Plug 'junegunn/fzf', { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" eye candy
Plug 'ryanoasis/vim-devicons'

call plug#end()

" optional packages in vim
packadd! matchit

" fix from git kitty issues 108
" vim hardcodes background color erase even if the terminfo file does
" not contain bce (not to mention that libvte based terminals
" incorrectly contain bce in their terminfo files). This causes
" incorrect background rendering when using a color theme with a
" background color.
if &term == 'xterm-kitty'
  let &t_ut=''
endif

" Filetype customization 
syntax enable
filetype plugin on
filetype indent on

" coloring and highlighting
set background=dark
colorscheme darkblue
set termguicolors
highlight Comment gui=italic cterm=italic

" nvim hack, guicursor applies to terminal mode as well???
" this disables changing the cursor to a thin vertical bar
" when in insert mode
set guicursor=  

" adjust gitgutter marks to use default retro colors.
" i'm still looking for a way to do this only if i
" know gitgutter is loaded
" TODO: this gets E254 cannot allocation color RetroBG & RetroFG in vim, not in nvim
"if exists('RetroBG')
"  highlight GitGutterAdd    guibg=RetroBG guifg=RetroFG
"  highlight GitGutterChange guibg=RetroBG guifg=RetroFG
"  highlight GitGutterDelete guibg=RetroBG guifg=RetroFG
"endif
" TODO: SignColumn gets messed up using DarkBlue scheme.
" highlight! link SIgnColumn LineNR fixes most, but marks
" from GitGutter are still wrong. Disabling GitGutter for
" now since I don't really use it.

" make mouse available in either vim or nvim
set mouse=a
"if !has('nvim')
"    set ttymouse=xterm2
"endif
"if has('nvim')
"    set mouse=a
"endif

" UI appearance and some behavior
set nocursorline                " Don't highlight current line
set tabpagemax=10               " Only show 10 tabs
set noshowmode                  " Statusline displays mode
set ruler                       " Show the ruler
set showcmd                     " Show partial commands in status line and
set laststatus=2
set hidden
set cmdheight=2                 " more space in command line
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
set sidescroll=8                " chunks
set nowrap                      " Do not wrap long lines

set foldenable                  " Auto fold code
set foldmethod=indent           " indent makes the most sense to me
set foldlevelstart=99           " open most folds when starting

" formatting defaults
set autoindent                  " Indent at the same level of the previous line
set copyindent                  " use tabs or spaces as on prior line
set shiftwidth=4                " Use indents of four spaces
set noexpandtab                 " let's try the go way
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
" set virtualedit=all           " i may want this for mainframe style editing, like the good old days

" TODO: ripgrep?
set grepprg=LC_ALL=C\ grep\ -nrsh

" TODO: is there a better way to access headers like <sys/types.h>
" for jumping to files? right now i'm hardcoding to my lubuntu system.
set path+=/usr/include/x86_64-linux-gnu
set path+=**                    " allow :e file autocomplete in subdirectories

" ----------------------------------------------------------------------------
" Key remaps
" ----------------------------------------------------------------------------

" ----------------------------------------------------------------------------
" my preferred leaders...
" be sure to unmap space, you'll see lags in insert mode otherwise
" ----------------------------------------------------------------------------
nnoremap <space> <nop>
let mapleader = " "
let maplocalleader="\\"

" ----------------------------------------------------------------------------
" from vimtips wiki, syntax highlighting group under cursor
" ----------------------------------------------------------------------------
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . "> trans<" . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" ----------------------------------------------------------------------------
" quickly edit and source .vimrc. this will not work with the $MYVIMRC
" variable in nvim since i actually use .vimrc, so the path is hard.
" coded.
" ----------------------------------------------------------------------------
nnoremap <leader>ev :vsplit ~/.vimrc<cr>
nnoremap <leader>sv :source ~/.vimrc<cr>

" ----------------------------------------------------------------------------
" move a line down or up. i'm not fond of these mappings.
" ----------------------------------------------------------------------------
nnoremap <leader>- ddp
nnoremap <leader>_ ddkkp

" ----------------------------------------------------------------------------
" uppercase current word. there is a bug if on first character of word.
" i haven't figured out how to use *, which correctly selects the word
" but does not seem to do what i need in visual mode.
" inoremap <leader><c-u> <esc>bveU<esc>wi
" ----------------------------------------------------------------------------
nnoremap <leader><c-u> bveU<esc>w

" ----------------------------------------------------------------------------
" wrap current word in single or double quotes.
" TODO: this word selection is what i need to understand for uppercase
" word above.
" ----------------------------------------------------------------------------
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

" ----------------------------------------------------------------------------
" wrap visual selection in single or double quotes. losh suggested using
" `< and `> but i couldn't find a way to preserve the selection with the
" movement. this works for me.
" ----------------------------------------------------------------------------
vnoremap <leader>" xi""<esc>hp
vnoremap <leader>' xi''<esc>hp

" hardcore mode mappings to get out of the habit of using esc and arrow
" keys:
"
" after furhter consideration, i'm going to keep the default key
" behavior for these. i don't need to be a speed demon these days.
"
" inoremap jk <esc>
" inoremap <esc> <nop>
" inoremap <up> <nop>
" inoremap <down> <nop>
" inoremap <left> <nop>
" inoremap <right> <nop>
" nnoremap <up> <nop>
" nnoremap <down> <nop>
" nnoremap <left> <nop>
" nnoremap <right> <nop>

" ----------------------------------------------------------------------------
" Quickfix
" ----------------------------------------------------------------------------
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
nnoremap ]l :lnext<cr>zz
nnoremap [l :lprev<cr>zz

" ----------------------------------------------------------------------------
" Buffers
" ----------------------------------------------------------------------------
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>

" ----------------------------------------------------------------------------
" Tabs
" ----------------------------------------------------------------------------
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>

" ----------------------------------------------------------------------------
" <tab> / <s-tab> | Circular windows navigation
" ----------------------------------------------------------------------------
nnoremap <tab>   <c-w>w
nnoremap <S-tab> <c-w>W

" ----------------------------------------------------------------------------
" ctags
" ----------------------------------------------------------------------------
map <leader>t :silent !ctags -R<CR><C_L>
" TODO: the following is c specific, need to consider generalization
map <leader>/ :execute "vimgrep // **/*[.ch]"

" ----------------------------------------------------------------------------
" abbreviations for typos  and common
" text.
" ----------------------------------------------------------------------------
iabbrev @@ blametroi@gmail.com
iabbrev ccopy Copyright 2022 Troy Brumley, all rights reserved.
iabbrev ssig -- <cr>Troy Brumley<cr>blametroi@gmail.com

" ----------------------------------------------------------------------------
" mappings and customization for specific filetypes
" ----------------------------------------------------------------------------
augroup filetype_basic
  autocmd!
  autocmd filetype basic nnoremap <buffer> <localleader>c I'<esc>
  autocmd BufNewFile,BufRead *.bas set ft=basic
  autocmd BufNewFile,BufRead *.bi  set ft=basic
  autocmd BufNewFile,BufRead *.bm  set ft=basic
  autocmd BufNewFile,BufRead *.bas compiler fbc
augroup END

augroup filetype_pascal
  autocmd!
  autocmd filetype pascal nnoremap <buffer> <localleader>c I//<esc>
augroup END

augroup filetype_c
  autocmd!
  autocmd filetype c nnoremap <buffer> <localleader>c I//<esc>
augroup END    

augroup filetype_vim
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END

augroup filetype_cobol
  autocmd!
  autocmd filetype cobol filetype indent off
augroup END
