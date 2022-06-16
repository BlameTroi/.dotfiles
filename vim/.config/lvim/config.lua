-- setting up lunarvim
-- pulling as much as possible out of vimrc. i thought about
-- keeping everything in there but lunarvim doesn't go after
-- neovim's init.vim so rather than change it to do so i'll
-- just convert to lua.

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = ","
vim.g.localleader = "\\"

-- general
lvim.log.level = "warn"
lvim.format_on_save = false
lvim.colorscheme = "dracula"
vim.g.loaded_perl_provider = 0

-- how i like to see things ...
vim.opt.cmdheight = 2
vim.opt.number = true
vim.opt.numberwidth = 6
vim.opt.incsearch = false
vim.opt.hlsearch = true
vim.opt.whichwrap = "b,s,h,l,<,>,[,]"
vim.opt.scrolljump = 1
vim.opt.scrolloff = 3
vim.opt.sidescroll = 8
vim.opt.foldenable = true
vim.opt.foldmethod = "indent"
vim.opt.foldlevelstart = 99

-- my general formatting defaults
vim.opt.wrap = false
vim.opt.autoindent = true
vim.opt.copyindent = true
vim.opt.shiftwidth = 4
vim.opt.expandtab = false
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.joinspaces = false

-- add your own keymapping
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"
-- unmap a default keymapping
-- vim.keymap.del("n", "<C-Up>")
-- override a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>" -- or vim.keymap.set("n", "<C-q>", ":q<cr>" )

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- we use protected-mode (pcall) just in case the plugin wasn't loaded yet.
-- local _, actions = pcall(require, "telescope.actions")
-- lvim.builtin.telescope.defaults.mappings = {
--   -- for input mode
--   i = {
--     ["<C-j>"] = actions.move_selection_next,
--     ["<C-k>"] = actions.move_selection_previous,
--     ["<C-n>"] = actions.cycle_history_next,
--     ["<C-p>"] = actions.cycle_history_prev,
--   },
--   -- for normal mode
--   n = {
--     ["<C-j>"] = actions.move_selection_next,
--     ["<C-k>"] = actions.move_selection_previous,
--   },
-- }

-- Use which-key to add extra bindings with the leader-key prefix
-- lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }
-- lvim.builtin.which_key.mappings["t"] = {
--   name = "+Trouble",
--   r = { "<cmd>Trouble lsp_references<cr>", "References" },
--   f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
--   d = { "<cmd>Trouble document_diagnostics<cr>", "Diagnostics" },
--   q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
--   l = { "<cmd>Trouble loclist<cr>", "LocationList" },
--   w = { "<cmd>Trouble workspace_diagnostics<cr>", "Wordspace Diagnostics" },
-- }

-- TODO: User Config for predefined plugins
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.notify.active = true
lvim.builtin.lualine.style = "default"
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = true
lvim.builtin.treesitter.highlight.enabled = true
-- if you don't want all the parsers change this to a table of
-- the ones you want
lvim.builtin.treesitter.ensure_installed = {
	"bash",
	"c",
	"javascript",
	"json",
	"lua",
	"python",
	"typescript",
	"rust",
	"java",
	"yaml",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true
lvim.builtin.treesitter.indent.disable = {"yaml", "python", "c", "cpp" }

-- Additional Plugins
lvim.plugins = {
	{ "Mofiqul/dracula.nvim" },
	-- {
	-- 	"folke/zen-mode.nvim",
	-- 	config = function()
	-- 		require("zen-mode").setup { plugins = { tmux = {enabled = true }}}
	-- 	end
	-- },
	-- {
	-- 	"folke/twilight.nvim",
	-- 	config = function()
	-- 		require("twilight").setup {
	-- 			-- add code here or leave empty for defaults
	-- 		}
	-- 	end
	-- },
	{
		"folke/lsp-colors.nvim"
	},
	{
		"folke/trouble.nvim",
		cmd = "TroubleToggle",
		config = function()
		require("trouble").setup {
				-- taking defaults right now
		}
		end
	},
	{ "editorconfig/editorconfig-vim" },
	-- {
	-- 	"tpope/vim-surround",
	-- 	keys = { "c", "d", "y" },
	-- 	config = function()
	-- 	require("vim-surround").setuup {
	-- 			-- not aware of any needed
	-- 	}
	-- 	end
	-- },
	{
		"ethanholz/nvim-lastplace",
		event = "BufRead",
		config = function()
			require("nvim-lastplace").setup({
				lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
				lastplace_ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit" },
				lastplace_open_folds = true,
			})
		end
	},
	{
		"TimUntersberger/neogit",
		config = function()
			require("neogit").setup{}
		end
	},
	{
		"nvim-neorg/neorg",
		tag = "*",
		config = function()
		require("neorg").setup {}
		end
	},
	{
		"chentoast/marks.nvim",
		config = function()
		require("marks").setup {
				default_mappings = true,
				builtin_marks = { ".", "<", ">", "^" },
		}
		end
	},
	{
		"mrjones2014/legendary.nvim",
		config = function ()
		require("legendary").setup({
				include_builtin = true,
				include_legendary_cmds = true,
				select_prompt = nil,
				formatter = nil,
				most_recent_item_at_top = true,
				keymaps = {},
				commands = {},
				autocmds = {},
				auto_register_which_key = true,
				scratchpad = {
					display_results = 'float',
				},
		})
		end
	},
	-- more to come...
}

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- vim.api.nvim_create_autocmd("BufEnter", {
--   pattern = { "*.json", "*.jsonc" },
--   -- enable wrap mode for json files only
--   command = "setlocal wrap",
-- })
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "zsh",
--   callback = function()
--     -- let treesitter use bash highlight for zsh files as well
--     require("nvim-treesitter.highlight").attach(0, "bash")
--   end,
-- })
