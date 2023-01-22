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

;; TODO: plans

;; faces:
;;   keywords  -- bold (green)
;;   builtins  -- bold (green)
;;   comments  -- italics (grey)
;;   strings   -- normal (?? color ??)
;;   literals  -- normal (?? color ??)
;;   otherwise -- normal

;; formatting:
;;   blocks       -- begin/end own lines, indented as well
;;   nested funcs -- indented
;;   if then/else -- as you would expect
;;   if /else if  -- if on same line as else, but not if else begin if
;;   case         -- line up with case
;;   semicolon    -- triggers new line
;;   end          -- triggers new line
;;   then/else    -- triggers new line
;;   operators    -- space surrounds
;;   parens       -- functions no space
;;                -- expressions leading space before (, trailing after )

;; formatting ...
;; line ups
;; spacing rules/consistency
;; function names always have no space before (,
;; semicolons no space before, always newline after
;; subscripts work the same as parens
;; +-/*<> and digraphs (here's where it gets tricky) are spaced around

;;; Notes:

;; I'm working through a few examples I found on the emacswiki.org site.
;; I expect this to be choppy.

;;; Code:

;; Be a good citizen and allow users to hook in.
(defvar fpc-mode-hook nil)

;; Define our keymap.
(defvar fpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";"        'electric-fpc-semi-or-dot)
    (define-key map "."        'electric-fpc-semi-or-dot)
    ;; (define-key map ":"        'electric-fpc-colon)
    ;; (define-key map "="        'electric-fpc-equal)
    ;; (define-key map "#"        'electric-fpc-hash)

    (define-key map "\r"       'electric-fpc-terminate-line)
    (define-key map "\t"       'electric-fpc-tab)
    (define-key map "\M-\t"    'completion-at-point)
    (define-key map "\M-?"     'completion-help-at-point)
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
    map)
  "Keymap used in fpc mode.")

;; syntax table ...
(defvar fpc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (setq fpc-mode-syntax-table (make-syntax-table))
    ;; why is backslash punctuation?
    (modify-syntax-entry ?\\ "."   st)
    ;; various forms of comments (* *) { }
  ;; TODO: why is the code i lifted from one of the pascal.el variants
  ;; doing just ?\{ "<" instead of "< b"/"<}b"/"<}" ???
  ;;  (modify-syntax-entry ?\{ "< b" fpc-mode-syntax-table)
  ;;  (modify-syntax-entry ?\} "> b" fpc-mode-syntax-table)
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
    (modify-syntax-entry ?@ "_"    st)
    (modify-syntax-entry ?^ "_"    st)
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
  "Syntax table in use in fpc-mode buffers.")

;;;
;;; tables lists and so on
;;;

;; this list is just for documentation
(defconst fpc-all-reserved-words
  '(
    ;; 'font-lock-keyword-face
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
    "procedure"
    "program"
    "repeat"
    "then"
    "to"
    "until"
    "while"
    "with"

    ;; 'font-lock-type-face
    "and"
    "array"
    "const"
    "div"
    "file"
    "in"
    "main"
    "mod"
    "nil"
    "not"
    "of"
    "or"
    "packed"
    "record"
    "set"
    "type"
    "var"

    ;; 'font-lock-builtin-face
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
    "integer"
    "ln"
    "maxint"
    "minint"
    "new"
    "odd"
    "open"
    "ord"
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

    ;; 'font-lock-doc-face
    "asm"
    "class"
    "define"
    "extern"
    "external"
    "finalization"
    "implementation"
    "initialization"
    "input"
    "interface"
    "module"
    "otherwise"
    "output"
    "private"
    "public"
    "static"
    "unit"
    "uses"

    ))

;; highlighting as optimized regexps

(defconst fpc-font-lock-keywords-list
    (list
     '("\\<\\(asm\\|begin\\|c\\(?:ase\\|lass\\)\\|d\\(?:efine\\|o\\(?:wnto\\)?\\)\\|e\\(?:lse\\|nd\\|xtern\\(?:al\\)?\\)\\|f\\(?:inalization\\|or\\(?:ward\\)?\\|unction\\)\\|goto\\|i\\(?:f\\|mplementation\\|n\\(?:itialization\\|put\\|terface\\)\\)\\|label\\|module\\|o\\(?:therwise\\|utput\\)\\|p\\(?:r\\(?:ivate\\|o\\(?:cedure\\|gram\\)\\)\\|ublic\\)\\|repeat\\|static\\|t\\(?:hen\\|o\\)\\|u\\(?:n\\(?:it\\|til\\)\\|ses\\)\\|w\\(?:hile\\|ith\\)\\)\\>" . font-lock-keyword-face)
     '("\\<\\(a\\(?:bs\\|nd\\|rctan\\|ssign\\)\\|boolean\\|c\\(?:h\\(?:a?r\\)\\|lose\\|os\\)\\|di\\(?:spose\\|v\\)\\|e\\(?:o\\(?:f\\|ln\\)\\|x\\(?:it\\|p\\)\\)\\|false\\|get\\|in\\(?:teger\\)?\\|ln\\|m\\(?:a\\(?:in\\|xint\\)\\|inint\\|od\\)\\|n\\(?:ew\\|il\\|ot\\)\\|o\\(?:dd\\|pen\\|rd\\|[fr]\\)\\|p\\(?:red\\|ut\\)\\|r\\(?:e\\(?:a\\(?:dln\\|[dl]\\)\\|s\\(?:\\(?:e\\|ul\\)t\\)\\|write\\)\\|ound\\)\\|s\\(?:in\\|qrt?\\|ucc\\)\\|t\\(?:an\\|ext\\|ru\\(?:e\\|nc\\)\\)\\|write\\(?:ln\\)?\\)\\>" . font-lock-builtin-face)
    '("\\<\\(?:array\\|const\\|file\\|packed\\|record\\|set\\|type\\|var\\)\\>" . font-lock-keyword-face)
    ;; following doesn't work, I'm not sure why yet ... does the italic
    ;; face need to be cloned and added to the font-lock-faces group? see various
    ;; stack exchange posts
    ;;  '("\\<\\(:=\\|and\\|div\\|mod\\|not\\|or\\|[!*+/=@^-]\\)//>" . italic)
    ;; '("\\<\\(:=\\|[!*+/=@^-]\\)//>" . font-lock-builtin-face)
    ;; perhaps it's the duplication of and/div/mod/not/or ? nope.
    ;; but ... order matters, moving this above builtins maybe?
))

;; TODO: can this be a constant?
(defvar fpc-font-lock-keywords fpc-font-lock-keywords-list "highlighting for fpc")

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst fpc-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst fpc-beg-block-re   "\\<\\(begin\\|case\\|record\\|repeat\\)\\>")
(defconst fpc-end-block-re   "\\<\\(end\\|until\\)\\>")
(defconst fpc-declaration-re "\\<\\(const\\|label\\|type\\|var\\)\\>")
(defconst fpc-progbeg-re     "\\<\\program\\>")
(defconst fpc-defun-re       "\\<\\(function\\|procedure\\|program\\)\\>")
(defconst fpc-sub-block-re   "\\<\\(if\\|else\\|for\\|while\\|with\\)\\>")
(defconst fpc-noindent-re    "\\<\\(begin\\|end\\|until\\|else\\)\\>")
(defconst fpc-nosemi-re      "\\<\\(begin\\|repeat\\|then\\|do\\|else\\)\\>")
(defconst fpc-autoindent-lines-re
  "\\<\\(label\\|var\\|type\\|const\\|until\\|end\\|begin\\|repeat\\|else\\)\\>")

;;; Strings used to mark beginning and end of excluded text
(defconst fpc-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst fpc-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

;;;
;;;  Macros
;;;

(defun fpc-declaration-end ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-forward
		 "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)"
		 (point-at-eol 2) t))
      (cond ((match-beginning 1) (setq nest (1+ nest)))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((looking-at "[^(\n]+)") (setq nest 0))))))


(defun fpc-declaration-beg ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-backward "[:=]\\|\\<\\(type\\|var\\|label\\|const\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (point-at-bol 0) t))
      (cond ((match-beginning 1) (setq nest 0))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((match-beginning 3) (setq nest (1+ nest)))))
    (= nest 0)))


(defsubst fpc-within-string ()
  (nth 3 (parse-partial-sexp (point-at-bol) (point))))


;;;
;;; Interactive functions
;;;

(defun fpc-insert-block ()
  "Insert fpc begin ... end; block in the code with right indentation."
  (interactive)
  (insert "begin")
  (electric-fpc-terminate-line)
  (save-excursion
    (newline)
    (insert "end;")
    (beginning-of-line)
    ;;(fpc-indent-line)
    ))

;;;
;;;  Electric functions
;;;
(defun electric-fpc-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at fpc-autoindent-lines-re)
	(fpc-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (fpc-indent-line)
  ;; Maybe we should set some endcomments
  ;;(if pascal-auto-endcomments
  ;;    (pascal-set-auto-comments))
  ;; Check if we shall indent inside comment
  ; (let ((setstar nil)) }
  ;   (save-excursion }
  ;     (forward-line -1) }
  ;     (skip-chars-forward " \t") }
  ;     (cond ((looking-at "\\*[ \t]+)") }
  ;        ;\;\ Delete region between `*' and `)' if there is only whitespaces. }
  ;        (forward-char 1) }
  ;        (delete-horizontal-space)) }
  ;       ((and (looking-at "(\\*\\|\\*[^)]") }
  ;   	  (not (save-excursion (search-forward "*)" (point-at-eol) t)))) }
  ;        (setq setstar t)))) }
  ;   ;\;\ If last line was a star comment line then this one shall be too. }
  ;   (if (null setstar) }
  ;   (pascal-indent-line) }
  ;     (insert "*  "))) }
  )


(defun electric-fpc-semi-or-dot ()
  "Insert `;' or `.' character and reindent the line."
  (interactive)
  (insert last-command-event)
  (save-excursion
    (beginning-of-line)
    (fpc-indent-line))
  (if fpc-auto-newline
      (electric-fpc-terminate-line)))

(defun electric-fpc-colon ()
  "Insert `:' and do all indentations except line indent on this line."
  (interactive)
  (insert last-command-event)
  ;; Do nothing if within string.
  (if (fpc-within-string)
      ()
    (save-excursion
      (beginning-of-line)
      (fpc-indent-line))
    (let ((fpc-tab-always-indent nil))
      (fpc-indent-command))))

(defun electric-fpc-equal ()
  "Insert `=', and do indentation if within type declaration."
  (interactive)
  (insert last-command-event)
  (if (eq (car (fpc-calculate-indent)) 'declaration)
      (let ((fpc-tab-always-indent nil))
	(fpc-indent-command))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pas\\'" . fpc-mode))
;; TODO: .pp .lpr, what aout delphi equivalents

;; old code to wire up the mode:
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
  (set (make-local-variable 'font-lock-defaults)
       '(fpc-font-lock-keywords nil t nil))
  (use-local-map fpc-mode-map)
  (set-syntax-table fpc-mode-syntax-table)
  (set (make-local-variable 'comment-start) "{")
  (set (make-local-variable 'comment-end) "}")


  ;; fpc-mode-hook should be called automatically by define-derived-node
  )


(provide 'fpc-mode)

;;; fpc.el ends here

