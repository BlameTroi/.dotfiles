" =============================================================================
" Name: retro-green
" Amber theme for Vim
" -----------------------------------------------------------------------------
"
" This is a theme that simulates the retro PC amber monochrome CRT. It has
" pretty much no syntax-specific variations in color and is, instead, focusing
" on providing variations for the editor elements themselves (e.g., line
" numbers, color column, etc).
" 
" This theme can toggle between light and dark mode by setting the `background`
" variable to "light" or "dark".
"
" Some tricks and code borrowed from Lucious theme by Jonathan Filip.
" =============================================================================

" txb 09/20/22 add search higlights and highlight searches

highlight clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name="green"

let s:everything = [
      \ "ColorColumn",
      \ "Comment",
      \ "Conceal",
      \ "Constant",
      \ "Cursor", 
      \ "CursorColumn",
      \ "CursorIM",
      \ "CursorLine",
      \ "CursorLineNr",
      \ "CursorLineNr",
      \ "DiffAdd", 
      \ "DiffChange",
      \ "DiffDelete",
      \ "DiffText",
      \ "Directory",
      \ "Error",
      \ "ErrorMsg", 
      \ "FoldColumn",
      \ "Folded", 
      \ "Identifier",
      \ "IncSearch",
      \ "LineNr",
      \ "MatchParen",
      \ "ModeMsg",
      \ "MoreMsg",
      \ "NonText",
      \ "Normal",
      \ "Pmenu",
      \ "PmenuSbar",
      \ "PmenuSel",
      \ "PmenuThumb",
      \ "PreProc",
      \ "Question",
      \ "Search",
      \ "SignColumn",
      \ "Special",
      \ "SpecialKey",
      \ "SpellBad", 
      \ "SpellCap",  
      \ "SpellLocal", 
      \ "SpellRare",
      \ "Statement",
      \ "StatusLine",
      \ "StatusLineNC",
      \ "TabLine",
      \ "TabLineFill",
      \ "TabLineSel",
      \ "Title",
      \ "Todo",
      \ "Type",
      \ "Underlined", 
      \ "VertSplit",
      \ "Visual",
      \ "VisualNOS",
      \ "WarningMsg",
      \ "WildMenu"
      \ ]

let s:inverted_items = [
      \ "Cursor",
      \ "CursorIM",
      \ "CursorColunmn",
      \ "CursorLineNr",
      \ "Visual", 
      \ "StatusLine"
      \ ]

let s:sub_inverted_items = [
      \ "VertSplit",
      \ "StatusLineNC",
      \ "PmenuSel"
      \ ]

let s:special_items = [
      \ "CursorLine",
      \ "ColorColumn",
      \ "NonText"
      \ ]

let s:error_items = [
      \ "SpellBad",
      \ "Error"
      \ ]

let s:style = &background

if s:style == "dark"
  let s:fg="#00d000"
  " go pure black
  let s:bg="#001000"
  " let s:bg="#000000"
  let s:special="#60ff60"
  let s:subbg="#80ff80"
  let RetroFG="#00d000"
  let RetroBG="#001000"
else
  " go pure black
  " let s:fg="#140b05"
  let s:fg="#000000"
  let s:bg="#fc9505"
  let s:special="#e58806"
  let s:subbg="#9e5d07"
  let RetroFG="#000000"
  let RetroBG="#fc9505"
endif

hi clear Normal

for s:item in s:everything
  exec "hi " . s:item . " guifg=" . s:fg . " guibg=" . s:bg .
        \ " gui=none ctermfg=NONE ctermbg=NONE cterm=none term=none"
      "   \ " gui=none ctermfg=" . s:fg . " ctermbg=" . s:bg . " cterm=none term=none"
endfor

for s:item in s:inverted_items
  exec "hi " . s:item . " guifg=" . s:bg . " guibg=" . s:fg 
endfor

for s:item in s:sub_inverted_items
  exec "hi " . s:item . " guifg=" . s:bg . " guibg=" . s:subbg 
endfor

for s:item in s:special_items
  exec "hi " . s:item . " guibg=" . s:special
endfor

for s:item in s:error_items
  exec "hi " . s:item . " guifg=#ff0000"
endfor

" tweaks, while colors follow into cterm from gui, i still want
" some additional hilighting
hi incsearch cterm=reverse gui=reverse
hi search    cterm=reverse gui=reverse
hi comment   cterm=italic  gui=italic
