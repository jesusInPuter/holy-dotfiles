return {
  "sainnhe/gruvbox-material",

  priority = 1000,

  -- This will load the plugin on startup
  lazy = false,

  config = function()
    -- Load the colorscheme here
    vim.cmd.colorscheme("gruvbox-material")

    -- vim.g.gruvbox_material_background = "hard"
  end,
}
