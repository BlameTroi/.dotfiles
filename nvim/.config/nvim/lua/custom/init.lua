vim.g.mapleader = ","
vim.g.localleader = "\\"
-- vim.cmd [[:echom "init.lua before"]]
-- vim.opt.whichwrap = "<>[]hl,b,w"
-- vim.cmd [[:echom "init.lua after"]]
require "custom.autocmds"
--vim.cmd "silent! command! EnableShade lua require('shade').toggle()"
