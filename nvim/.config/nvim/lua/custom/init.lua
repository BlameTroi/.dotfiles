vim.g.mapleader = ","
vim.g.localleader = "\\"
-- vim.cmd [[:echom "init.lua before"]]
-- vim.opt.whichwrap = "<>[]hl,b,w"
-- vim.cmd [[:echom "init.lua after"]]
vim.cmd [[:echom "init-one"]]
require "custom.autocmds"
vim.cmd [[:echom "init-two"]]
--vim.cmd "silent! command! EnableShade lua require('shade').toggle()"
