
-- with lazy.nvim
return{
  "kylechui/nvim-surround",
  version = "*", -- Use latest stable
  event = "VeryLazy",
  config = function()
    require("nvim-surround").setup({})
  end
}
