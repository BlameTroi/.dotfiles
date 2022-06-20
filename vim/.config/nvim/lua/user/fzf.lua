local status_ok, fzf = pcall(require, "fzf")
if not status_ok then
  return
end

fzf.setup {
	-- what to do here?
}
