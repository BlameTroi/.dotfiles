# Notes from a gist by Joshua Moyers

Taken from https://gist.github.com/jmoyers/de19d4a6050dcfe67d110782346496ce
This was referenced in a youtube video describing his minimalistic use of
these tools.

- color scheme
  - dracula - https://draculatheme.com/
    - tmux
    - terminal
    - vim
  - using plug (vim), tpm (tmux), and a json file for windows terminal
- terminal
  - i use bash, you can use zsh, its all mostly applicable
  - fuzzy learning cd: https://github.com/rupa/z
  - z proj, takes you to /some/deep/directory/project
- tmux
  - not required if you prefer using your terminals built-in windowing scheme
  - i use it to have consistent keybinds for switching between windows and tabs
  - detach/attach are nice for saving sessions
- vim
  - basic set of motions
  - useful motions
    - h, j, k, l
    - ciw, ci(, ci{
    - viw, shift-v, ctrl-v, =
    - f{c}, F{c}
    - :line-number
    - G, gg
    - ctrl-d, ctrl-u
    - /search
    - I, i, o, O, A
    - :w, :q
  - fuzzy file open for vim: https://github.com/kien/ctrlp.vim
  - :Make and :Dispatch (build & run) https://github.com/tpope/vim-dispatch
  - try built in :make (lower case) to see what the differences are
  - vim quickfix window via :Make
    - ctrl-w ctrl-w toggles between windows
    - enter or :cc takes you to error
    - :cn goes to next error, :cp goes to prev
    - or just hjkl in the quickfix window and hit enter
  - optional but nice
    - vim plugin manager: https://github.com/junegunn/vim-plug
- debugger
  - gdb over lldb for me for now due to default "visual" support
  - lldb equivalent to gdb tui can be whipped together with https://github.com/snare/voltron
  - lldb built-in tui is not that easy to use and feels wonky to me
  - gdb tui (text user interface) mode (tui enable, layout src/asm/regs)
  - mvp gdb commands
      - tui enable
      - layout src
      - layout asm
      - layout reg
      - b {func/source:line} - breakpoint
      - delete breakpoints
      - n - step over
      - s - step in, si - step instruction for asm
      - p {var}, ptype {var} - print variable or variable type info
      - display {var} - show variable on every step
      - r - restart/run
  - build
    - for smaller or scratch project: g++/clang++ directly
    - for larger project: cmake or bazel
    - cmake (and im sure bazel) works great with :Make and vim quickfix
- advanced & extra
  - clang formatter - stop arguing about code formatting rules
    - https://clang.llvm.org/docs/ClangFormat.html
    - vim plugin: https://github.com/rhysd/vim-clang-format
  - google test - https://github.com/google/googletest - unit testing for c++
  - google benchmark - https://github.com/google/benchmark - a micro benchmarking library
  - clang sanitizers
    - example: https://clang.llvm.org/docs/MemorySanitizer.html
    - need to build libc++ and clang from source https://github.com/llvm/llvm-project
      - they have a nice CMake build, its not that bad
    - need to link to libc++ as std library, load appropriate headers for std library
    - more instructions here: https://github.com/google/sanitizers/wiki/MemorySanitizerLibcxxHowTo1
    - DONT RUN SCRIPTS FROM HERE, but you can examine! https://github.com/jmoyers/dotfiles
