-- This variable holds the buffer ID of the terminal window,
-- allowing the function to reuse the same terminal across multiple runs.
local term_buf_id = nil

---
-- Prepares a terminal window for the command.
-- It will reuse the existing terminal if available, or create a new one.
-- @return (number|nil) The job_id of the terminal, or nil if it fails.
local function setup_terminal()
  -- Check if the stored buffer ID is still valid
  if term_buf_id and vim.api.nvim_buf_is_valid(term_buf_id) then
    vim.cmd("wincmd l") -- Focus the window to the right (the terminal)
    vim.cmd("vertical resize 45")
  else
    -- If no valid terminal exists, create a new one
    vim.cmd("vsplit")
    vim.cmd("vertical resize 70")
    vim.cmd("terminal fish")
    term_buf_id = vim.api.nvim_get_current_buf() -- Save the new buffer's ID
  end

  -- Get the job ID from the terminal buffer
  local job_id = vim.b[term_buf_id].terminal_job_id
  if not job_id then
    vim.notify("Could not get a valid terminal job ID.", vim.log.levels.ERROR)
    return nil
  end

  return job_id
end

---
-- Builds the complete shell command string to be executed.
-- @param filename (string) The name of the file (without extension).
-- @return (string) The shell command.
local function build_command(filename)
  local command_parts = {
    "clear",
    "echo ''",
    "echo ''",
    "echo '=== Running: " .. filename .. ".cpp ==='",
    "echo ''",
    "echo '--- Compiling ---'",
    "echo ''",
    "make " .. filename,
    "echo '--- Executing ---'",
    "echo ''",
    "./" .. filename,
    "echo ''",
    "echo '--- Finished ---'",
    "echo ''",
    "echo 'cleaning binaries : " .. filename .. "'",
    "rm " .. filename,
  }

  -- Chain commands with '&&' so the sequence stops if any command fails
  return table.concat(command_parts, " && ") .. "\n"
end

---
-- The main function to orchestrate compiling and running the C++ file.
local function run_cpp()
  vim.cmd("write") -- 1. Save the current file

  local filename = vim.fn.expand("%:r")
  if filename == "" then
    vim.notify("Cannot run a file without a name.", vim.log.levels.WARN)
    return
  end

  -- 2. Get the terminal ready
  local job_id = setup_terminal()
  if not job_id then return end

  -- 3. Build and send the command
  local command = build_command(filename)
  vim.fn.chansend(job_id, command)

  -- 4. Return focus to the original code window
  vim.cmd("wincmd h")
end

-- Make the main function globally accessible for the keymap
_G.run_cpp = run_cpp

-- Set the keymap to trigger the function
vim.api.nvim_set_keymap("n", "<leader>mm", "<cmd>lua run_cpp()<CR>", { noremap = true, silent = true })
