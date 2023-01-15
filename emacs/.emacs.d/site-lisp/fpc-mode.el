;;; fpc.el --- major mode for lazarus and free pascal  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: languages

;; This file is for use in GNU Emacs.

;; I'm building a major more for Pascal programming as I like to code, starting
;; from scratch but heavily influenced by (and sometimes outright copying code
;; from) the distributed pascal.el. With that in mind:

;; Some of the following is from pascal.el, authored by Espen Skoglund
;; <esk@gnu.org>, and is Copyright (C) 1993-2022 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; USAGE
;; =====

;; Override the use of the normal pascal or opascal modes for Pascalish
;; files. File extensions .pas, .pp, and .lpr work for Free Pascal and
;; Lazarus, and there are parallels extensions for Delphi.

;; The goal is a not too intrusive mode geared for reading as much as
;; writing code. Not a lot of bells and whistles, but hopefully a
;; consistent user experience.

;;; Notes:

;; I'm working through a few examples I found on the emacswiki.org site.
;; I expect this to be choppy.

;;; Code:

;; Be a good citizen and allow users to hook in.
(defvar fpc-mode-hook nil)

;; Define our keymap.
(defvar fpc-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map ";"        'electric-fpc-semi-or-dot)
    ;; (define-key map "."        'electric-fpc-semi-or-dot)
    ;; (define-key map ":"        'electric-fpc-colon)
    ;; (define-key map "="        'electric-fpc-equal)
    ;; (define-key map "#"        'electric-fpc-hash)
    ;; ;; These are user preferences, so not to set by default.
    ;; (define-key map "\r"       'electric-fpc-terminate-line)
    ;; (define-key map "\t"       'electric-fpc-tab)
    ;; (define-key map "\M-\t"    'completion-at-point)
    ;; (define-key map "\M-?"     'completion-help-at-point)
    ;; (define-key map "\177"     'backward-delete-char-untabify)
    ;; (define-key map "\M-\C-h"  'fpc-mark-defun)
    (define-key map "\C-c\C-b" 'fpc-insert-block)
    ;; (define-key map "\M-*"     'fpc-star-comment)
    ;; (define-key map "\C-c\C-c" 'fpc-comment-area)
    ;; (define-key map "\C-c\C-u" 'fpc-uncomment-area)
    ;; (define-key map "\M-\C-a"  'fpc-beg-of-defun)
    ;; (define-key map "\M-\C-e"  'fpc-end-of-defun)
    ;; (define-key map "\C-c\C-d" 'fpc-goto-defun)
    ;; (define-key map "\C-c\C-o" 'fpc-outline-mode)
    ;; A command to change the whole buffer won't be used terribly
    ;; often, so no need for a key binding.
    ;; (define-key map "\C-cd"    'fpc-downcase-keywords)
    ;; (define-key map "\C-cu"    'fpc-upcase-keywords)
    ;; (define-key map "\C-cc"    'fpc-capitalize-keywords)
    map)
  "Keymap used in fpc mode.")

;; syntax table ...
(defvar fpc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (setq pascal-mode-syntax-table (make-syntax-table))
    ;; why is backslash punctuation?
    (modify-syntax-entry ?\\ "."   st)
    ;; various forms of comments (* *) { }
    (modify-syntax-entry ?\( "()1"  st)
    (modify-syntax-entry ?\) ")(4"  st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?{ "<"    st)
    (modify-syntax-entry ?} ">"    st)
    ;; most operator characters are punctuation
    (modify-syntax-entry ?+ "."    st)
    (modify-syntax-entry ?- "."    st)
    (modify-syntax-entry ?= "."    st)
    (modify-syntax-entry ?% "."    st)
    (modify-syntax-entry ?< "."    st)
    (modify-syntax-entry ?> "."    st)
    (modify-syntax-entry ?& "."    st)
    (modify-syntax-entry ?| "."    st)
    ;; underscores are word characters
    (modify-syntax-entry ?_ "w"    st)
    (modify-syntax-entry ?. "_"    st)
    ;; pascal strings are in single quotes
    (modify-syntax-entry ?\' "\""  st)
    (modify-syntax-entry ?/' ". 124b" st)
    ;; a new line can end some comments, but
    ;; the following breaks some things
    ;; (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table in use in Pascal-mode buffers.")

;;;
;;; tables lists and so on
;;;

(defconst fpc-all-reserved-words
  '(

    ;; this list is mostly for documentation purposes ...
    ;; standard keywords

    "and"
    "array"
    "begin"
    "case"
    "const"
    "div"
    "do"
    "downto"
    "else"
    "end"
    "file"
    "for"
    "forward"
    "function"
    "goto"
    "if"
    "in"
    "label"
    "main"
    "mod"
    "nil"
    "not"
    "of"
    "or"
    "packed"
    "procedure"
    "program"
    "record"
    "repeat"
    "set"
    "then"
    "to"
    "type"
    "until"
    "var"
    "while"
    "with"

    ;; predefined identifiers, i/o, dynamic memory, built in functions ...

    "abs"
    "arctan"
    "boolean"
    "char"
    "chr"
    "close"
    "cos"
    "dispose"
    "eof"
    "eoln"
    "exit"
    "exp"
    "false"
    "get"
    "input"
    "integer"
    "ln"
    "maxint"
    "minint"
    "new"
    "odd"
    "open"
    "ord"
    "output"
    "pred"
    "put"
    "read"
    "readln"
    "real"
    "reset"
    "result"
    "rewrite"
    "round"
    "sin"
    "sqr"
    "sqrt"
    "succ"
    "tan"
    "text"
    "true"
    "trunc"
    "write"
    "writeln"

    ;; tp, units, linkage, objects, and so on

    "asm"
    "class"
    "define"
    "extern"
    "external"
    "finalization"
    "implementation"
    "initialization"
    "interface"
    "module"
    "otherwise"
    "private"
    "public"
    "static"
    "unit"
    "uses"

    ))

(defconst fpc-keywords-unopt
  '(
    ;; an unoptimized list of words to highlight as reserved
    "begin"
    "case"
    "do"
    "downto"
    "else"
    "end"
    "for"
    "forward"
    "function"
    "goto"
    "if"
    "label"
    "main"
    "procedure"
    "program"
    "repeat"
    "then"
    "to"
    "until"
    "while"
))

(defconst fpc-font-lock-keywords-1
    ;; use regexp-opt on prior lists to create better faster exps
    (list
     '("\\<\\(program\\|procedure\\|function\\|begin\\|end\\|if\\|then\\|else\\|case\\|repeat\\|until\\|while\\|do\\|with\\|downto\\|to\\|goto\\)\\>" . font-lock-keyword-face)
     '("\\<\\(type\\|record\\|integer\\|string\\|real\\|boolean\\|char\\|var\\)\\>" . font-lock-variable-face)
     ))

(defvar fpc-font-lock-keywords fpc-font-lock-keywords-1 "lockem")
;;;
;;; Interactive Functions
;;;

;;;
;;; Interactive functions
;;;

(defun fpc-insert-block ()
  "Insert fpc begin ... end; block in the code with right indentation."
  (interactive)
  (insert "begin")
  ;;(electric-fpc-terminate-line)
  (save-excursion
    (newline)
    (insert "end;")
    (beginning-of-line)
    ;;(fpc-indent-line)
    ))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pas\\'" . fpc-mode))
;; TODO: .pp .lpr, what aout delphi equivalents

;; Wire the mode up:
;; (defun fpc-mode ()
;;   "Major mode for editing Free Pascal and Lazarus files"
;;   (interactive)
;;   (kill-all-local-variables)
;;   ;;(set-syntax-table fpc-mode-syntax-table)
;;   (use-local-map fpc-mode-map)
;;   ;;(set (make-local-variable 'font-lock-defaults) '(fpc-mode-font-lock-keywords))
;;   (setq major-mode 'fpc-mode)
;;   (setq mode-name "fpc")
;;   (run-hooks 'fpc-mode-hook))

(define-derived-mode fpc-mode prog-mode "fpc"
  "fpc mode is a major mode for editing Pascal files with an eye
toward Lazarus, Free Pascal, and Object Pascal."
  (set (make-local-variable 'font-lock-defaults) '(fpc-font-lock-keywords))
  (use-local-map fpc-mode-map)
  (set-syntax-table fpc-mode-syntax-table)
  (setq comment-start "{")
  (setq comment-end "}")
  (modify-syntax-entry ?\{ "< b" fpc-mode-syntax-table)
  (modify-syntax-entry ?\} "> b" fpc-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  fpc-mode-syntax-table)
  ;; fpc-mode-hook should be called automatically by define-derived-node
  )


(provide 'fpc-mode)

;;; fpc.el ends here

