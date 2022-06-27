local M = {}

-- TODO: add keys for symbols-outline.nvim
-- :SymbolsOutline or :SymbolsOutline{Open|Close}
-- toggle is probably sufficient

--
-- M.disabled = {
--    "<C-n>",
-- }

vim.cmd [[:echom "mappings general"]]
M.general = {

  i = {
    -- hardcore keys to train me not to use arrow keys
    ["jk"] = { "<ESC>", "" },
    -- ["<up>"] = { "<nop>", "" },
    -- ["<down>"] = { "<nop>", "" },
    -- ["<left>"] = { "<nop>", "" },
    -- ["<right>"] = { "<nop>", "" },
  },

  -- n = {
  --   -- hardcore keys to train me not to use arrow keys
  --   ["<up>"] = { "<nop>", "" },
  --   ["<down>"] = { "<nop>", "" },
  --   ["<left>"] = { "<nop>", "" },
  --   ["<right>"] = { "<nop>", "" },
  -- },
}

vim.cmd [[:echom "mapping treesitter"]]
M.treesitter = {
   n = {
      ["<leader>cu"] = { "<cmd> TSCaptureUnderCursor <CR>", "  find media" },
   },
}

vim.cmd [[:echom "mapping done"]]
return M
