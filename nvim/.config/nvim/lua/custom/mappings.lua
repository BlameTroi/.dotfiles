local M = {}

--
-- M.disabled = {
--    "<C-n>",
-- }

M.general = {

  i = {
    -- hardcore keys to train me not to use arrow keys
    ["jk"] = { "<ESC>", "" },
    ["<up>"] = { "<nop>", "" },
    ["<down>"] = { "<nop>", "" },
    ["<left>"] = { "<nop>", "" },
    ["<right>"] = { "<nop>", "" },
  },

  n = {
    -- hardcore keys to train me not to use arrow keys
    ["<up>"] = { "<nop>", "" },
    ["<down>"] = { "<nop>", "" },
    ["<left>"] = { "<nop>", "" },
    ["<right>"] = { "<nop>", "" },
  },
}

M.treesitter = {
   n = {
      ["<leader>cu"] = { "<cmd> TSCaptureUnderCursor <CR>", "  find media" },
   },
}

return M
