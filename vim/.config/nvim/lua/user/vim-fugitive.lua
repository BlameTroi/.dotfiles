local status_ok, fugitive = pcall(require, "vim-fugitive")
if not status_ok then
  return
end

fugitive.setup {
	-- what to do here?
}
