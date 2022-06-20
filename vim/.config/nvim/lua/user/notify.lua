-- this doesn't work, just leaving it as a shell
-- TODO: figure this out.
local plugin = {}

plugin.core = {
	"rcarriga/nvim-notify",
	setup = function()
		-- any setup here?
	end,

	config = function()
		vim.notify = require("notify")
		-- anything here?
	end,
}

plugin.mapping = function()
	-- anything here?
end
return plugin
