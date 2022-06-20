--[[ init.lua ]]
--
-- t.brumley, june 2022
--
-- migrating to nvim but slowly as i trip through the configuration landscape
-- and discover various gotchas. the best resources i've found for aiding my
-- understanding and some cookbook code/structure are:
--
-- https://github.com/thesoomyung/neovim-config
-- https://mattermost.com/blog/how-to-install-and-set-up-neovim-for-code-editing/
-- https://git.sr.ht/~theorytoe/nii-nvim
-- https://tkg.codes/guide-to-modern-neovim-setup-2021/
--
-- and i'm probably missing a few. neovimcraft is a good starting point as well.

-- [[ things that must be done early ]]
-- multiple posts recommend setting leaders early. i've also added a setting to
-- not use a perl provider. i'm not using perl and this removes an error in
-- :checkhealth.
vim.g.loaded_perl_provider = 0
vim.g.mapleader = ","
vim.g.localleader = "\\"

-- [[ options and mappings not dependent on plugins ]]
require "user.options"
require "user.keymaps"

-- [[ packer bootstrap and plugin configuration ]]
-- user.plugins will bootstrap packer on first run.
require "user.plugins"

-- [[ theme and colors ]]
require "user.colorscheme"

-- [[ ui general ]]
require "user.nvim-tree"
require "user.lualine"
require "user.bufferline"
-- TODO: bqf should lazyload, but i don't see how that all works with the config
--       add "ft = 'qf'" to the use in plugins.lua will the bqf plugin to opt
require "user.bqf"
require "user.nvim-lastplace"
-- popup and dressing don't require setup at this time
-- require "user.notify" -- TODO: not working yet
-- require "user.toggleterm"

-- require "user.indentline"

-- [[ bookmarks, motion, and navigation ]]
require "user.marks"
require "user.hop"

-- [[ telescope & search ]]
require "user.fzf"
require "user.telescope"

-- [[ completion ]]
-- require "user.cmp"

-- [[ lsp configuration ]]
-- following goes into the user/lsp directory to execute init.lua
-- require "user.lsp"
require "user.treesitter"
-- require "user.autopairs"
-- -- require "user.project"
-- -- require "user.impatient"
-- [[ edit helpers ]]
require "user.comment"

-- [[ git ]]
require "user.vim-fugitive"

-- [[ key mapping ]]
require "user.whichkey"
-- TODO: mappings in wk format are in the above, do i move others there?
-- TODO: see ignore_missing and show_help in setup(), should i change to not hide?

-- [[ autocommands and final tweaks ]]
-- require "user.autocommands"
