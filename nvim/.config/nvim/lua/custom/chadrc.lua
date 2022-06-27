local M = {}

local override = require "custom.override"

vim.cmd [[:echom "chadrc options"]]
M.options = {
	user = function()
		require "custom.options"
	end,

	nvChad = {
		update_url = "https://github.com/NvChad/NvChad",
		update_branch = "main",
	},

}

vim.cmd [[:echom "chadrc plugins"]]
M.plugins = {

	options = {
		lspconfig = {
			setup_lspconf = "custom.plugins.lspconfig",
		},

		statusline = {
			separator_style = "round",
		},
	},

	override = {
		["kyazdani42/nvim-tree.lua"] = override.nvimtree,
		["nvim-treesitter/nvim-treesitter"] = override.treesitter,
		["lukas-reineke/indent-blankline.nvim"] = override.blankline,
	},

	user = require "custom.plugins",
}

vim.cmd [[:echom "chadrc mapping"]]
M.mappings = require "custom.mappings"

vim.cmd [[:echom "chadrc ui"]]
M.ui = {
	theme = "chadracula",

	statusline = {
		separator_style = "arrow", -- default/round/block/arrow
		config = "%!v:lua.require('ui.statusline').run()",
		override = {},
	},

	hl_override = require "custom.highlights",

	tabufline_enabled = true,
}

-- vim.cmd [[:echom "chadrc mapping"]]
-- M.mappings = require "custom.mappings"

vim.cmd [[:echom "chadrc done"]]
return M
