-- This file simply bootstraps the installation of Lazy.nvim and then calls other files for execution
-- This file doesn't necessarily need to be touched, BE CAUTIOUS editing this file and proceed at your own risk.
local lazypath = vim.env.LAZY or vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not (vim.env.LAZY or (vim.uv or vim.loop).fs_stat(lazypath)) then
  -- stylua: ignore
  local result = vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath })
  if vim.v.shell_error ~= 0 then
    -- stylua: ignore
    vim.api.nvim_echo(
      { { ("Error cloning lazy.nvim:\n%s\n"):format(result), "ErrorMsg" }, { "Press any key to exit...", "MoreMsg" } },
      true, {})
    vim.fn.getchar()
    vim.cmd.quit()
  end
end

vim.opt.rtp:prepend(lazypath)

-- validate that lazy is available
if not pcall(require, "lazy") then
  -- stylua: ignore
  vim.api.nvim_echo(
    { { ("Unable to load lazy from: %s\n"):format(lazypath), "ErrorMsg" }, { "Press any key to exit...", "MoreMsg" } },
    true, {})
  vim.fn.getchar()
  vim.cmd.quit()
end

require "lazy_setup"
require "polish"

-- Keymaps
vim.keymap.set("n", "<leader>dd", ":DevdocsOpenCurrentFloat<CR>", { silent = true })

vim.opt.termguicolors = true
vim.opt.showtabline = 0

local conform = require("conform")

conform.setup({
  -- Defines the formatters to use for each file type.
  formatters_by_ft = {
    -- Use clang-format for C and C++ files.
    c = { "clang_format" },
    cpp = { "clang_format" },

  },

  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
}) -- install without yarn or npm
vim.g.vimtex_view_method = 'zathura'
vim.g.vimtex_compiler_method = 'latexmk'

-- Warnings
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

vim.keymap.set("n", "<leader>tw", ToggleWarnings, { desc = "Toggle Warnings" })

-- Enable inlay hints on attach
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(args)
    if not (args.data and args.data.client_id) then
      return
    end

    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end
  end,
})
