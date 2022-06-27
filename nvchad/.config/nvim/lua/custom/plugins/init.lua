return {

   ["nvim-neorg/neorg"] = {
      ft = "norg",
      after = "nvim-treesitter",
      config = function()
         require "custom.plugins.neorg"
      end,
   },

   ["ethanholz/nvim-lastplace"] = {
     config = function()
       require "custom.plugins.nvim-lastplace"
     end,
   },

   ["chentoast/marks.nvim"] = {
     config = function()
       require "custom.plugins.marks"
     end,
   },

   ["andweeb/presence.nvim"] = {
   },

   ["stevearc/dressing.nvim"] = {
   },

   ["simrat39/symbols-outline.nvim"] = {
   },

   ["p00f/nvim-ts-rainbow"] = {
   }

}
