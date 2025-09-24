return {
  "nvim-telescope/telescope.nvim",

  keys = {
    { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Find Recent Files" },
    -- { "<leader>ff", false },
  },

  -- The 'opts' table will be intelligently merged with the default options.
  opts = {
    defaults = {
      layout_strategy = "vertical",
      layout_config = {
        vertical = {
          preview_cutoff = 0,
        },
      },
      -- And change the sorting
      sorting_strategy = "ascending",
    },
  },
}
