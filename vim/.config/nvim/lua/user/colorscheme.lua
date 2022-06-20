vim.cmd [[
set termguicolors
try
  colorscheme dracula
  set background=dark
catch /^Vim\%((\a\+)\)\=:E185/
  colorscheme default
  set background=dark
endtry
highlight comment cterm=italic, gui=italic
syntax enable
filetype plugin on
filetype indent on
]]
