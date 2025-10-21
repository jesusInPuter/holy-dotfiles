-- ----------------------------------------------------------------------
-- Lazy.nvim installation path
-- ----------------------------------------------------------------------
local lazypath = vim.env.LAZY or (vim.fn.stdpath("data") .. "/lazy/lazy.nvim")

-- ----------------------------------------------------------------------
-- Bootstrap Lazy.nvim (if not already installed)
-- ----------------------------------------------------------------------
if not (vim.env.LAZY or (vim.uv or vim.loop).fs_stat(lazypath)) then
  local result = vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })

  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { ("Error cloning lazy.nvim:\n%s\n"):format(result), "ErrorMsg" },
      { "Press any key to exit...", "MoreMsg" },
    }, true, {})
    vim.fn.getchar()
    vim.cmd.quit()
  end
end

-- ----------------------------------------------------------------------
-- Add Lazy.nvim to runtime path
-- ----------------------------------------------------------------------
vim.opt.rtp:prepend(lazypath)

-- ----------------------------------------------------------------------
-- Validate Lazy.nvim load
-- ----------------------------------------------------------------------
if not pcall(require, "lazy") then
  vim.api.nvim_echo({
    { ("Unable to load lazy from: %s\n"):format(lazypath), "ErrorMsg" },
    { "Press any key to exit...", "MoreMsg" },
  }, true, {})
  vim.fn.getchar()
  vim.cmd.quit()
end

-- ----------------------------------------------------------------------
-- Load core Lazy setup & polish
-- ----------------------------------------------------------------------

require("lazy_setup")
require("polish")
-- ======================================================================
--  General Editor Settings
-- ======================================================================
vim.opt.termguicolors = true
vim.opt.showtabline = 0
vim.opt.signcolumn = 'yes'
-- ======================================================================
--  Keymaps
-- ======================================================================
vim.keymap.set("n", "<leader>dd", ":DevdocsOpenCurrentFloat<CR>", { silent = true, desc = "Open DevDocs Float" })
vim.keymap.set("n", "<leader>tw", function() ToggleWarnings() end, { desc = "Toggle LSP Warnings" })

-- ======================================================================
--  Formatter Setup (conform.nvim)
-- ======================================================================
local conform = require("conform")

conform.setup({
  formatters_by_ft = {
    c = { "clang_format" },
    cpp = { "clang_format" },
  },
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

-- ======================================================================
--  LaTeX (vimtex) Configuration
-- ======================================================================
vim.g.vimtex_view_method = "zathura"
vim.g.vimtex_compiler_method = "latexmk"

-- ======================================================================
--  LSP Warning Controls
-- ======================================================================
local show_warnings = true

function ToggleWarnings()
  show_warnings = not show_warnings
  vim.diagnostic.config({
    virtual_text = {
      severity = show_warnings and nil or { min = vim.diagnostic.severity.ERROR },
    },
    signs = {
      severity = show_warnings and nil or { min = vim.diagnostic.severity.ERROR },
    },
    underline = {
      severity = show_warnings and nil or { min = vim.diagnostic.severity.ERROR },
    },
  })
end

-- ======================================================================
--  LSP Inlay Hints (Auto-enable on attach)
-- ======================================================================
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(args)
    if not (args.data and args.data.client_id) then return end

    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end
  end,
})

-- ======================================================================
--  Load Custom User Config
-- ======================================================================
require("custom")
