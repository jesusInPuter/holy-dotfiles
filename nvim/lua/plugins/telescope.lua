return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      cond = function()
        return vim.fn.executable "make" == 1
      end,
    },
  },
  keys = {
    -- Find files
    { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files" },
    -- Find text in current project (live grep)
    { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep" },
    -- Find open buffers
    { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Find Buffers" },
    -- Find recent files
    { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Find Recent Files" },
  },
  opts = {
    defaults = {
      -- Use the 'flex' layout strategy for a centered, modern UI
      layout_strategy = "flex",
      layout_config = {
        flex = {
          -- Preview should be on the right, always.
          horizontal = {
            preview_width = 0.55,
          },
        },
      },
      sorting_strategy = "ascending",
      prompt_prefix = "  ",
      selection_caret = "󰋖 ",
    },
  },
  config = function(_, opts)
    local telescope = require "telescope"
    telescope.setup(opts)
    telescope.load_extension "fzf"
  end,
}
