vim.opt.cmdheight = 2
vim.opt.number = true
vim.opt.numberwidth = 6
vim.opt.incsearch = true
vim.opt.hlsearch = true
vim.opt.scrolljump = 1
vim.opt.scrolloff = 3
vim.opt.sidescroll = 8
vim.opt.foldenable = true
vim.opt.foldmethod = "expr" -- use treesitter
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevelstart = 99
vim.opt.hidden = true -- required to keep multiple buffers and open multiple buffers,
vim.opt.signcolumn = "yes"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.wrap = false
vim.opt.autoindent = true
vim.opt.copyindent = true
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.joinspaces = false

vim.cmd [[
syntax enable
filetype on
filetype plugin on
filetype indent on
highlight comment cterm=italic, gui=italic
]]

vim.cmd [[
iabbrev @@ blametroi@gmail.com
iabbrev ccopy Copyright 2022 Troy Brumley, all rights reserved.
iabbrev ssig -- <cr>Troy Brumley<cr>blametroi@gmail.com
]]
