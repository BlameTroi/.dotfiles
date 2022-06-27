local M = {}

local override = require "custom.override"

M.options = {
	user = function()
		require "custom.options"
	end,

	nvChad = {
		update_url = "https://github.com/NvChad/NvChad",
		update_branch = "main",
	},

}

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

M.mappings = require "custom.mappings"

return M
