return {

  "andweeb/presence.nvim",
  {
    "ray-x/lsp_signature.nvim",
    lazy = false,
    event = "BufRead",
    config = function() require("lsp_signature").setup() end,
  },

  {
    "windwp/nvim-autopairs",
    config = function(plugin, opts)
      require "astronvim.plugins.configs.nvim-autopairs" (plugin, opts) -- default config

      local npairs = require "nvim-autopairs"
      local Rule = require "nvim-autopairs.rule"
      local cond = require "nvim-autopairs.conds"

      npairs.add_rules(
        {
          Rule("$", "$", { "tex", "latex" })
              :with_pair(cond.not_after_regex "%%")
              :with_pair(cond.not_before_regex("xxx", 3))
              :with_move(cond.none())
              :with_del(cond.not_after_regex "xx")
              :with_cr(cond.none()),
        },
        Rule("a", "a", "-vim")
      )

      -- Minimal highlight for the active pair
      vim.cmd [[
      hi! link NPairsMatch CursorLineNr
    ]]
      npairs.setup {
        highlight = "NPairsMatch",  -- highlight active pair
        highlight_grey = "Comment", -- non-active pairs dimmed
      }
    end,
  },
}
