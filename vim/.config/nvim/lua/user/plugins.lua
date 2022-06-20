local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	}
	print "Installing packer close and reopen Neovim..."
	vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
  autocmd!
  autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init {
	display = {
		open_fn = function()
			return require("packer.util").float { border = "rounded" }
		end,
	},
}

-- Install your plugins here
return packer.startup(function(use)
	-- [[ packer can manage itself after boostrap ]]
	use "wbthomason/packer.nvim" -- Have packer manage itself

	-- [[ utility and uncategorized ]]
	use "nvim-lua/plenary.nvim" -- Useful lua functions used by lots of plugins
	use "blametroi/autotags" -- a personal port

	-- [[ theme and colors ]]
	use "Mofiqul/dracula.nvim"

	-- [[ general ui behavior and eye candy ]]
	use "kyazdani42/nvim-web-devicons"
	use "kyazdani42/nvim-tree.lua"
	use "nvim-lualine/lualine.nvim"
	use "akinsho/bufferline.nvim"
	use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
	use "stevearc/dressing.nvim"
	use "ethanholz/nvim-lastplace"
	-- use "rcarriga/nvim-notify" -- not working yet TODO: fix it
	use "kevinhwang91/nvim-bqf"
	-- TODO: bqf broken on sync 06/20/22, we'll live without for a while
	-- use {
	-- 	"kevinhwang91/nvim-bqf",
	-- 	-- TODO: ft = "qf" -- will lead to lazy loading and move to opt, need to get this working 
	-- }
	use "akinsho/toggleterm.nvim"

	-- [[ bookmarks, motion, and navigation ]]
	use "chentoast/marks.nvim"
	use { "phaazon/hop.nvim", branch = "v1" }

	-- cmp plugins
	--  use "hrsh7th/nvim-cmp" -- The completion plugin
	--  use "hrsh7th/cmp-buffer" -- buffer completions
	--  use "hrsh7th/cmp-path" -- path completions
	--  use "hrsh7th/cmp-cmdline" -- cmdline completions
	--  use "saadparwaiz1/cmp_luasnip" -- snippet completions
	--  use "hrsh7th/cmp-nvim-lsp"

	-- snippets, editing support aside from completion
	use "L3MON4D3/LuaSnip" --snippet engine
	use "rafamadriz/friendly-snippets" -- a bunch of snippets to use
	use "numToStr/Comment.nvim" -- Easily comment stuff

	-- LSP
	use "neovim/nvim-lspconfig" -- enable LSP
	use "williamboman/nvim-lsp-installer" -- simple to use language server installer
	use "tamago324/nlsp-settings.nvim" -- language server settings defined in json for
	use "jose-elias-alvarez/null-ls.nvim" -- for formatters and linters

	-- Telescope & search
	use "junegunn/fzf"
	use "junegunn/fzf.vim"
	use "nvim-telescope/telescope.nvim"
	use {
		"nvim-telescope/telescope-fzf-native.nvim",
		run = "make"
	}
	use "fcying/telescope-ctags-outline.nvim"

	-- Treesitter
	use {
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	}
	use "p00f/nvim-ts-rainbow"
	use "JoosepAlviste/nvim-ts-context-commentstring"

	-- Git
	use "lewis6991/gitsigns.nvim"
	-- use "tpope/vim-fugitive"

	-- cheatsheets and documentation
	use {
		'sudormrfbin/cheatsheet.nvim',
		requires = {
			{'nvim-telescope/telescope.nvim'},
			{'nvim-lua/popup.nvim'},
			{'nvim-lua/plenary.nvim'}
		}
	}

	-- key mapping
	use 'folke/which-key.nvim'

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
