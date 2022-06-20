local status_ok, marks = pcall(require, "marks")
if not status_ok then
  return
end

marks.setup {
	default_mappings = true,
	builtin_marks = { ".", "<", ">", "^" },
	cyclic = true,
}
