local Websocket = require('websocket').Websocket

if SOCKET and SOCKET.client then
  SOCKET:close()
end

---@diagnostic disable-next-line: missing-fields
local socket = Websocket:new({
  host = "0.0.0.0",
  port = 8080,
  path = "/ws"
})

socket:add_on_message(vim.schedule_wrap(function(frame)
  local str = frame.payload
  print("RECV: " .. vim.inspect(vim.json.decode(str)))
end))

socket:add_on_connect(vim.schedule_wrap(function()
  print("CONNECTED")
  socket:send_text(vim.json.encode { event = "Hello", hello = true })
end))

socket:connect()

local group = vim.api.nvim_create_augroup("nvim-subd", { clear = true })
-- vim.api.nvim_create_autocmd("BufEnter", {
--   group = group,
--   callback = function()
--     if socket.client then
--       socket:send_text(vim.json.encode {
--         event = "BufEnter",
--         buffer = vim.api.nvim_get_current_buf(),
--         bufname = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()),
--       })
--     end
--   end
-- })

vim.api.nvim_create_user_command("SetScene", function(args)
  if socket.client then
    socket:send_text(vim.json.encode {
      event = "SetScene",
      scene = args.args
    })
  end
end, { nargs = 1 })

vim.api.nvim_create_user_command("WebEcho", function(args)
  if socket.client then
    socket:send_text(vim.json.encode {
      event = "Echo",
      data = args.args
    })
  end
end, { nargs = 1 })

SOCKET = socket