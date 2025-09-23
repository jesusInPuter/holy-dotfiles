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

local function ensure_cmake_configured()
  local build_dir = "build"
  local compile_json = build_dir .. "/compile_commands.json"
  if vim.fn.isdirectory(build_dir) == 0 or vim.fn.filereadable(compile_json) == 0 then
    vim.cmd("!cmake -S . -B " .. build_dir .. " -G Ninja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
    vim.cmd("!ln -sf " .. compile_json .. " ./compile_commands.json")
  end
end

-- Build only
vim.api.nvim_create_user_command("Build", function()
  ensure_cmake_configured()
  vim.cmd("!cmake --build build")
end, {})

-- Build and run default target (main)
vim.api.nvim_create_user_command("BR", function()
  ensure_cmake_configured()
  vim.cmd("!cmake --build build && ./build/main")
end, {})

-- Keymaps
vim.keymap.set("n", "<leader>b", ":Build<CR>", { silent = false })
vim.keymap.set("n", "<leader>r", ":BR<CR>", { silent = false })
vim.keymap.set("n", "<leader>dd", ":DevdocsOpenFloat<CR>", { silent = true })

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
