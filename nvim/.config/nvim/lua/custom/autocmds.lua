local autocmd = vim.api.nvim_create_autocmd

local opt = vim.opt

autocmd("FileType", {
  pattern = "norg",
  callback = function()
    -- vim.opt.laststatus = 0
    opt.number = false
    opt.showtabline = 0
    opt.cole = 1
    opt.foldlevel = 10
  end,
})

autocmd("FileType", {
  pattern = "lua",
  callback = function()
    opt.expandtab = true
    opt.shiftwidth = 2
    opt.tabstop = 2
    opt.softtabstop = 2
  end,
})

-- groups, defined via viml since there's chatter about the
-- api call nvim_create_augroup in multiple issues on github.

-- edit empty creates the new file
vim.cmd [[
augroup troi_global
autocmd!
autocmd FileType * set fo-=cro
autocmd BufNewFile * :write
augroup END
]]

-- specific file type settings
vim.cmd [[
augroup filetype_basic
autocmd!
autocmd BufNewFile,BufRead *.bas set ft=basic
autocmd BufNewFile,BufRead *.bi  set ft=basic
autocmd BufNewFile,BufRead *.bm  set ft=basic
autocmd BufNewFile,BufRead *.bas compiler fbc
augroup END

augroup filetype_pascal
autocmd!
autocmd BufNewFile,BufRead *.pas compiler fpc
augroup END
]]
