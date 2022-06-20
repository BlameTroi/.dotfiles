-- [[ lua/usr/options.lua ]]
-- vim options (set/let) as i like them.
local options = {
	-- backup = false,                          -- creates a backup file
	clipboard = "unnamedplus",               -- allows neovim to access the system clipboard
	cmdheight = 2,                           -- more space in the neovim command line for displaying messages
	completeopt = { "menuone", "noselect" }, -- mostly just for cmp
	conceallevel = 0,                        -- so that `` is visible in markdown files
	cursorline = true,                       -- highlight cursor line
	encoding = "utf-8",
	fileencoding = "utf-8",                  -- the encoding written to a file
	-- hlsearch = true,                         -- highlight all matches on previous search pattern
	incsearch = false,                       -- preference
	ignorecase = true,                       -- ignore case in search patterns
	showmatch = true,                        -- show matching brackets/parens
	mouse = "a",                             -- allow the mouse to be used in neovim
	pumheight = 10,                          -- pop up menu height
	-- showmode = false,                        -- we don't need to see things like -- INSERT -- anymore
	showtabline = 1,                         -- show tabs if more than one
	smartcase = true,                        -- smart case
	splitbelow = true,                       -- force all horizontal splits to go below current window
	splitright = true,                       -- force all vertical splits to go to the right of current window
	-- swapfile = false,                        -- creates a swapfile
	termguicolors = true,                    -- set term gui colors (most terminals support this)
	timeoutlen = 500,                        -- time to wait for a mapped sequence to complete (in milliseconds)
	undofile = true,                         -- enable persistent undo
	updatetime = 300,                        -- faster completion (4000ms default)
	writebackup = false,                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
	cursorline = true,                       -- highlight the current line
	number = true,                           -- set numbered lines
	relativenumber = false,                  -- set relative numbered lines
	numberwidth = 6,                         -- set number column width to 2 {default 4}
	signcolumn = "yes",                      -- always show the sign column, otherwise it would shift the text each time
	wrapscan = false,                        -- 
	scrolloff = 3,                           -- is one of my fav
	sidescrolloff = 8,
	-- guifont = "monospace:h17",               -- the font used in graphical neovim applications
	exrc = true,                             -- allow a per directory/project .exrc
	secure = true,                           -- but limit it's powers
	-- my default formatting
	wrap = false,                            -- display lines as one long line
	autoindent = true,                       -- indent at same level as previous line
	copyindent = true,                       -- and use tabs or spaces on on previous
	shiftwidth = 4,                          -- the number of spaces inserted for each indentation
	tabstop = 4,                             -- insert 2 spaces for a tab
	softtabstop = 4,                         -- backspace can delete indent
	-- smartindent = true,                   -- make indenting smarter again
	expandtab = false,                       -- convert tabs to spaces
	joinspaces = false,                      -- don't insert 2 spaces after punctuation on join line
}

vim.opt.shortmess:append "c"

for k, v in pairs(options) do
	vim.opt[k] = v
end

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]

-- TODO: does this belong in color scheme?
vim.cmd "highlight comment cterm=italic, gui=italic"

-- vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work
