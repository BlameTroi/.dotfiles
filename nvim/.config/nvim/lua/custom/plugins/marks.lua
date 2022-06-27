
local present, marks = pcall(require, "marks")

if not present then
  return
end

local options = {
  default_mappings = true,
  builtin_marks = { ".", "<", ">", "^" },
}

marks.setup(options)
