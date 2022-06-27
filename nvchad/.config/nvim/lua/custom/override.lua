-- overriding default plugin configs!

local M = {}

M.treesitter = {
    ensure_installed = {
        "c", "cpp", "bash", "cmake", "go", "gomod", "json", "javascript",
        "lua", "pascal", "python", "vim", "html", "http", "norg",
        "make", "fortran",
    },
    rainbow = {
      enable = true,
    },
}

M.nvimtree = {
    git = {
        enable = true,
    },

    renderer = {
        highlight_git = true,
        icons = {
            show = {
                git = true,
            },
        },
    },
}

M.blankline = {
    filetype_exclude = {
        "help",
        "terminal",
        "packer",
        "lspinfo",
        "TelescopePrompt",
        "TelescopeResults",
        "nvchad_cheatsheet",
        "lsp-installer",
        "norg",
        "",
    },
}

return M
