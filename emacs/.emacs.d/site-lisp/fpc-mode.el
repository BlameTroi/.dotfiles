;;; fpc-mode.el --- major mode for editing pascal source in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 1993-2013 Free Software Foundation, Inc.

;; Author: Espen Skoglund <esk@gnu.org>
;; Keywords: languages

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; USAGE
;; =====

;; Emacs should enter Pascal mode when you find a Pascal source file.
;; When you have entered Pascal mode, you may get more info by pressing
;; C-h m. You may also get online help describing various functions by:
;; C-h f <Name of function you want described>

;; If you want to customize Pascal mode to fit you better, you may add
;; these lines (the values of the variables presented here are the defaults):
;;
;; ;; User customization for Pascal mode
;; (setq fpc-indent-level       2
;;       fpc-case-indent        2
;;       fpc-auto-newline       nil
;;       fpc-tab-always-indent  t
;;       fpc-auto-endcomments   t
;;       fpc-auto-lineup        '(all)
;;       fpc-type-keywords      '("array" "file" "packed" "char"
;; 				     "integer" "real" "string" "record")
;;       fpc-start-keywords     '("begin" "end" "function" "procedure"
;; 				     "repeat" "until" "while" "read" "readln"
;; 				     "reset" "rewrite" "write" "writeln")
;;       fpc-separator-keywords '("downto" "else" "mod" "div" "then"))

;; KNOWN BUGS / BUGREPORTS
;; =======================
;; As far as I know, there are no bugs in the current version of this
;; package.  This may not be true however, since I never use this mode
;; myself and therefore would never notice them anyway.   If you do
;; find any bugs, you may submit them to: esk@gnu.org as well as to
;; bug-gnu-emacs@gnu.org.


;;; Code:

(defgroup fpc-mode nil
  "Major mode for editing fpc source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar fpc-mode-abbrev-table nil
  "Abbrev table in use in Fpc-mode buffers.")
(define-abbrev-table 'fpc-mode-abbrev-table ())

(defvar fpc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";"        'electric-fpc-semi-or-dot)
    (define-key map "."        'electric-fpc-semi-or-dot)
    (define-key map ":"        'electric-fpc-colon)
    (define-key map "="        'electric-fpc-equal)
    (define-key map "#"        'electric-fpc-hash)
    (define-key map "\r"       'electric-fpc-terminate-line)
    (define-key map "\t"       'electric-fpc-tab)
    (define-key map "\M-\t"    'completion-at-point)
    (define-key map "\M-?"     'completion-help-at-point)
    (define-key map "\177"     'backward-delete-char-untabify)
    (define-key map "\M-\C-h"  'fpc-mark-defun)
    (define-key map "\C-c\C-b" 'fpc-insert-block)
    (define-key map "\M-*"     'fpc-star-comment)
    (define-key map "\C-c\C-c" 'fpc-comment-area)
    (define-key map "\C-c\C-u" 'fpc-uncomment-area)
    (define-key map "\M-\C-a"  'fpc-beg-of-defun)
    (define-key map "\M-\C-e"  'fpc-end-of-defun)
    (define-key map "\C-c\C-d" 'fpc-goto-defun)
    (define-key map "\C-c\C-o" 'fpc-outline-mode)
    ;; A command to change the whole buffer won't be used terribly
    ;; often, so no need for a key binding.
    ;; (define-key map "\C-cd"    'fpc-downcase-keywords)
    ;; (define-key map "\C-cu"    'fpc-upcase-keywords)
    ;; (define-key map "\C-cc"    'fpc-capitalize-keywords)
    map)
  "Keymap used in fpc mode.")

(defvar fpc-imenu-generic-expression
  '((nil "^[ \t]*\\(function\\|procedure\\)[ \t\n]+\\([a-zA-Z0-9_.:]+\\)" 2))
  "Imenu expression for fpc-mode.  See `imenu-generic-expression'.")

(defvar fpc-keywords
  '("and" "array" "begin" "case" "const" "div" "do" "downto" "else" "end"
    "file" "for" "function" "goto" "if" "in" "label" "mod" "nil" "not" "of"
    "or" "packed" "procedure" "program" "record" "repeat" "set" "then" "to"
    "type" "until" "var" "while" "with"
    ;; The following are not standard in pascal, but widely used.
    "get" "put" "input" "output" "read" "readln" "reset" "rewrite" "write"
    "writeln"
    ;; more
    "assign" "uses" "interface" "implementation" "initialization" "finalization"
    ))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst fpc-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst fpc-beg-block-re   "\\<\\(begin\\|case\\|record\\|repeat\\)\\>")
(defconst fpc-end-block-re   "\\<\\(end\\|until\\)\\>")
(defconst fpc-declaration-re "\\<\\(const\\|label\\|type\\|var\\)\\>")
(defconst fpc-progbeg-re     "\\<\\program\\|interface\\>")
(defconst fpc-defun-re       "\\<\\(function\\|procedure\\|program\\)\\>")
(defconst fpc-sub-block-re   "\\<\\(if\\|else\\|for\\|while\\|with\\)\\>")
;; (defconst fpc-noindent-re    "\\<\\(begin\\|end\\|until\\|else\\)\\>")
(defconst fpc-noindent-re    "\\<\\(until\\|else\\)\\>")
(defconst fpc-nosemi-re      "\\<\\(begin\\|repeat\\|then\\|do\\|else\\)\\>")
(defconst fpc-autoindent-lines-re
  "\\<\\(label\\|var\\|type\\|const\\|until\\|end\\|begin\\|repeat\\|else\\|uses\\)\\>")

;;; Strings used to mark beginning and end of excluded text
(defconst fpc-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst fpc-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

(defvar fpc-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "."   st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    ;; This used to use comment-syntax `b'.  But the only document I could
    ;; find about the syntax of Pascal's comments said that (* ... } is
    ;; a valid comment, just as { ... *) or (* ... *) or { ... }.
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?{ "<"    st)
    (modify-syntax-entry ?} ">"    st)
    (modify-syntax-entry ?+ "."    st)
    (modify-syntax-entry ?- "."    st)
    (modify-syntax-entry ?= "."    st)
    (modify-syntax-entry ?% "."    st)
    (modify-syntax-entry ?< "."    st)
    (modify-syntax-entry ?> "."    st)
    (modify-syntax-entry ?& "."    st)
    (modify-syntax-entry ?| "."    st)
    (modify-syntax-entry ?_ "_"    st)
    (modify-syntax-entry ?\' "\""  st)
    st)
  "Syntax table in use in Fpc-mode buffers.")

(defconst fpc-font-lock-keywords
  (purecopy
   (list
    '("^[ \t]*\\(function\\|pro\\(cedure\\|gram\\)\\)\\>[ \t]*\\([a-z]\\)"
      1 font-lock-keyword-face)
    '("^[ \t]*\\(function\\|pro\\(cedure\\|gram\\)\\)\\>[ \t]*\\([a-z][a-z0-9_]*\\)"
      3 font-lock-function-name-face t)
                                        ;   ("type" "const" "real" "integer" "char" "boolean" "var"
                                        ;    "record" "array" "file")
    (cons (concat "\\<\\(array\\|boolean\\|c\\(har\\|onst\\)\\|file\\|"
		          "integer\\|re\\(al\\|cord\\)\\|type\\|var\\)\\>")
	      'font-lock-type-face)
    '("\\<\\(label\\|external\\|forward\\)\\>" . font-lock-constant-face)
    '("\\<\\([0-9]+\\)[ \t]*:" 1 font-lock-function-name-face)
                                        ;   ("of" "to" "for" "if" "then" "else" "case" "while"
                                        ;    "do" "until" "and" "or" "not" "in" "with" "repeat" "begin" "end")
    (concat "\\<\\("
	        "and\\|begin\\|case\\|do\\|e\\(lse\\|nd\\)\\|for\\|i[fn]\\|"
	        "not\\|o[fr]\\|repeat\\|t\\(hen\\|o\\)\\|until\\|w\\(hile\\|ith\\)"
	        "\\)\\>")
    '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
      1 font-lock-keyword-face)
    '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
      2 font-lock-keyword-face t)))
  "Additional expressions to highlight in fpc mode.")
(put 'fpc-mode 'font-lock-defaults '(fpc-font-lock-keywords nil t))

(defcustom fpc-indent-level 2
  "Indentation of fpc statements with respect to containing block."
  :type 'integer
  :group 'fpc-mode)

(defcustom fpc-case-indent 2
  "Indentation for case statements."
  :type 'integer
  :group 'fpc-mode)

(defcustom fpc-auto-newline nil
  "Non-nil means automatically insert newlines in certain cases.
These include after semicolons and after the punctuation mark after an `end'."
  :type 'boolean
  :group 'fpc-mode)

(defcustom fpc-indent-nested-functions t
  "Non-nil means nested functions are indented."
  :type 'boolean
  :group 'fpc-mode)

(defcustom fpc-tab-always-indent t
  "Non-nil means TAB in fpc mode should always reindent the current line.
If this is nil, TAB inserts a tab if it is at the end of the line
and follows non-whitespace text."
  :type 'boolean
  :group 'fpc-mode)

(defcustom fpc-auto-endcomments t
  "Non-nil means automatically insert comments after certain `end's.
Specifically, this is done after the ends of cases statements and functions.
The name of the function or case is included between the braces."
  :type 'boolean
  :group 'fpc-mode)

(defcustom fpc-auto-lineup '(all)
  "List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively.  The word 'all' will do all lineups.  '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups."
  :type '(set :extra-offset 8
	          (const :tag "Everything" all)
	          (const :tag "Parameter lists" paramlist)
	          (const :tag "Declarations" declaration)
	          (const :tag "Case statements" case))
  :group 'fpc-mode)

(defvar fpc-toggle-completions nil
  "If non-nil, `fpc-complete-word' tries all possible completions.
Repeated use of \\[fpc-complete-word] then shows all
completions in turn, instead of displaying a list of all possible
completions.")
(make-obsolete-variable 'fpc-toggle-completions
                        'completion-cycle-threshold "24.1")

(defcustom fpc-type-keywords
  '("array" "file" "packed" "char" "integer" "real" "string" "record")
  "Keywords for types used when completing a word in a declaration or parmlist.
These include integer, real, char, etc.
The types defined within the Pascal program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'fpc-mode)

(defcustom fpc-start-keywords
  '("begin" "end" "function" "procedure" "repeat" "until" "while"
    "read" "readln" "reset" "rewrite" "write" "writeln")
  "Keywords to complete when standing at the first word of a statement.
These are keywords such as begin, repeat, until, readln.
The procedures and variables defined within the Pascal program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'fpc-mode)

(defcustom fpc-separator-keywords
  '("downto" "else" "mod" "div" "then")
  "Keywords to complete when NOT standing at the first word of a statement.
These are keywords such as downto, else, mod, then.
Variables and function names defined within the Pascal program
are handled in another way, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'fpc-mode)

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

;;;###autoload
(define-derived-mode fpc-mode prog-mode "fpc"
  "Major mode for editing Pascal code. \\<fpc-mode-map>
TAB indents for Pascal code.  Delete converts tabs to spaces as it moves back.

\\[completion-at-point] completes the word around current point with respect \
to position in code
\\[completion-help-at-point] shows all possible completions at this point.

Other useful functions are:

\\[fpc-mark-defun]\t- Mark function.
\\[fpc-insert-block]\t- insert begin ... end;
\\[fpc-star-comment]\t- insert (* ... *)
\\[fpc-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[fpc-uncomment-area]\t- Uncomment an area commented with \
\\[fpc-comment-area].
\\[fpc-beg-of-defun]\t- Move to beginning of current function.
\\[fpc-end-of-defun]\t- Move to end of current function.
\\[fpc-goto-defun]\t- Goto function prompted for in the minibuffer.
\\[fpc-outline-mode]\t- Enter `fpc-outline-mode'.

Variables controlling indentation/edit style:

 `fpc-indent-level' (default 2)
    Indentation of Pascal statements with respect to containing block.
 `fpc-case-indent' (default 2)
    Indentation for case statements.
 `fpc-auto-newline' (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 `fpc-indent-nested-functions' (default t)
    Non-nil means nested functions are indented.
 `fpc-tab-always-indent' (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `fpc-auto-endcomments' (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 `fpc-auto-lineup' (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables `fpc-type-keywords', `fpc-start-keywords' and
`fpc-separator-keywords'.

Turning on fpc mode calls the value of the variable fpc-mode-hook with
no args, if that value is non-nil."
  (set (make-local-variable 'local-abbrev-table) fpc-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) 'fpc-indent-line)
  (set (make-local-variable 'comment-indent-function) 'fpc-indent-comment)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'blink-matching-paren-dont-ignore-comments) t)
  (set (make-local-variable 'case-fold-search) t)
  (set (make-local-variable 'comment-start) "{")
  (set (make-local-variable 'comment-start-skip) "(\\*+ *\\|{ *")
  (set (make-local-variable 'comment-end) "}")
  (add-hook 'completion-at-point-functions 'fpc-completions-at-point nil t)
  ;; Font lock support
  (set (make-local-variable 'font-lock-defaults)
       '(fpc-font-lock-keywords nil t))
  ;; Imenu support
  (set (make-local-variable 'imenu-generic-expression)
       fpc-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) t)
  ;; Fpc-mode's own hide/show support.
  (add-to-invisibility-spec '(fpc-mode . t)))

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
  (if fpc-auto-endcomments
      (fpc-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]+)")
	         ;; Delete region between `*' and `)' if there is only whitespaces.
	         (forward-char 1)
	         (delete-horizontal-space))
	        ((and (looking-at "(\\*\\|\\*[^)]")
		          (not (save-excursion (search-forward "*)" (point-at-eol) t))))
	         (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
    (if (null setstar)
	    (fpc-indent-line)
      (insert "*  "))))

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

(defun electric-fpc-hash ()
  "Insert `#', and indent to column 0 if this is a CPP directive."
  (interactive)
  (insert last-command-event)
  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
      (save-excursion (beginning-of-line)
		              (delete-horizontal-space))))

(defun electric-fpc-tab ()
  "Function called when TAB is pressed in Pascal mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (fpc-within-string)
	      (and (not (bolp))
	           (save-excursion (beginning-of-line) (eq (following-char) ?#))))
      (insert "\t")
    ;; If fpc-tab-always-indent, indent the beginning of the line.
    (if fpc-tab-always-indent
	    (save-excursion
	      (beginning-of-line)
	      (fpc-indent-line))
      (if (save-excursion
	        (skip-chars-backward " \t")
	        (bolp))
	      (fpc-indent-line)
	    (insert "\t")))
    (fpc-indent-command)))



;;;
;;; Interactive functions
;;;
(defvar fpc--extra-indent 0)

(defun fpc-insert-block ()
  "Insert fpc begin ... end; block in the code with right indentation."
  (interactive)
  (insert "begin")
  (electric-fpc-terminate-line)
  (save-excursion
    (newline)
    (insert "end;")
    (beginning-of-line)
    (fpc-indent-line)))

(defun fpc-star-comment ()
  "Insert fpc star comment at point."
  (interactive)
  (fpc-indent-line)
  (insert "(*")
  (electric-fpc-terminate-line)
  (save-excursion
    (electric-fpc-terminate-line)
    (delete-horizontal-space)
    (insert ")"))
  (insert "  "))

(defun fpc-mark-defun ()
  "Mark the current fpc function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (fpc-end-of-defun)
  (push-mark (point))
  (fpc-beg-of-defun)
  (when (featurep 'xemacs)
    (zmacs-activate-region)))

(defun fpc-comment-area (start end)
  "Put the region into a Pascal comment.
The comments that are in this area are \"deformed\":
`*)' becomes `!(*' and `}' becomes `!{'.
These deformed comments are returned to normal if you use
\\[fpc-uncomment-area] to undo the commenting.

The commented area starts with `fpc-exclude-str-start', and ends with
`fpc-include-str-end'.  But if you change these variables,
\\[fpc-uncomment-area] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	         (not (save-excursion (skip-chars-backward " \t") (bolp))))
	    (forward-line 1)
      (beginning-of-line))
    (insert fpc-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert fpc-exclude-str-start)
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*)" start t)
	    (replace-match "!(*" t t)))
    (save-excursion
      (while (re-search-backward "}" start t)
	    (replace-match "!{" t t)))))

(defun fpc-uncomment-area ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `fpc-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
	      (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	    (setq start (progn (search-backward fpc-exclude-str-start nil t)
			               (point)))
	    (setq end (progn (search-forward fpc-exclude-str-end nil t)
			             (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	      (message "Not standing within commented area.")
	    (progn
	      ;; Remove endcomment
	      (goto-char end)
	      (beginning-of-line)
	      (let ((pos (point)))
	        (end-of-line)
	        (delete-region pos (1+ (point))))
	      ;; Change comments back to normal
	      (save-excursion
	        (while (re-search-backward "!{" start t)
	          (replace-match "}" t t)))
	      (save-excursion
	        (while (re-search-backward "!(\\*" start t)
	          (replace-match "*)" t t)))
	      ;; Remove startcomment
	      (goto-char start)
	      (beginning-of-line)
	      (let ((pos (point)))
	        (end-of-line)
	        (delete-region pos (1+ (point)))))))))

(defun fpc-beg-of-defun ()
  "Move backward to the beginning of the current function or procedure."
  (interactive)
  (catch 'found
    (if (not (looking-at (concat "\\s \\|\\s)\\|" fpc-defun-re)))
	    (forward-sexp 1))
    (let ((nest 0) (max -1) (func 0)
	      (reg (concat fpc-beg-block-re "\\|"
		               fpc-end-block-re "\\|"
		               fpc-defun-re)))
      (while (re-search-backward reg nil 'move)
	    (cond ((let ((state (save-excursion
			                  (parse-partial-sexp (point-min) (point)))))
		         (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	           ())
	          ((match-end 1)                       ; begin|case|record|repeat
	           (if (and (looking-at "\\<record\\>") (>= max 0))
		           (setq func (1- func)))
	           (setq nest (1+ nest)
		             max (max nest max)))
	          ((match-end 2)                       ; end|until
	           (if (and (= nest max) (>= max 0))
		           (setq func (1+ func)))
	           (setq nest (1- nest)))
	          ((match-end 3)                       ; function|procedure
	           (if (= 0 func)
		           (throw 'found t)
		         (setq func (1- func)))))))
    nil))

(defun fpc-end-of-defun ()
  "Move forward to the end of the current function or procedure."
  (interactive)
  (if (looking-at "\\s ")
      (forward-sexp 1))
  (if (not (looking-at fpc-defun-re))
      (fpc-beg-of-defun))
  (forward-char 1)
  (let ((nest 0) (func 1)
	    (reg (concat fpc-beg-block-re "\\|"
		             fpc-end-block-re "\\|"
		             fpc-defun-re)))
    (while (and (/= func 0)
		        (re-search-forward reg nil 'move))
      (cond ((let ((state (save-excursion
			                (parse-partial-sexp (point-min) (point)))))
		       (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	         ())
	        ((match-end 1)
	         (setq nest (1+ nest))
	         (if (save-excursion
		           (goto-char (match-beginning 0))
		           (looking-at "\\<record\\>"))
		         (setq func (1+ func))))
	        ((match-end 2)
	         (setq nest (1- nest))
	         (if (= nest 0)
		         (setq func (1- func))))
	        ((match-end 3)
	         (setq func (1+ func))))))
  (forward-line 1))

(defun fpc-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((parse-sexp-ignore-comments t)
	    (nest 0) pos
	    (regexp (concat "\\(" fpc-beg-block-re "\\)\\|\\("
			            fpc-end-block-re "\\)")))
    (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
    (or (looking-at fpc-beg-block-re)
	    ;; Skip to end of statement
	    (setq pos (catch 'found
		            (while t
		              (forward-sexp 1)
		              (cond ((looking-at "[ \t]*;")
			                 (skip-chars-forward "^;")
			                 (forward-char 1)
			                 (throw 'found (point)))
			                ((save-excursion
			                   (forward-sexp -1)
			                   (looking-at fpc-beg-block-re))
			                 (goto-char (match-beginning 0))
			                 (throw 'found nil))
			                ((eobp)
			                 (throw 'found (point))))))))
    (if (not pos)
	    ;; Skip a whole block
	    (catch 'found
	      (while t
	        (re-search-forward regexp nil 'move)
	        (setq nest (if (match-end 1)
			               (1+ nest)
			             (1- nest)))
	        (cond ((eobp)
		           (throw 'found (point)))
		          ((= 0 nest)
		           (throw 'found (fpc-end-of-statement))))))
      pos)))

;; (defun fpc-downcase-keywords ()
;;   "Downcase all Pascal keywords in the buffer."
;;   (interactive)
;;   (fpc-change-keywords 'downcase-word))

;; (defun fpc-upcase-keywords ()
;;   "Upcase all Pascal keywords in the buffer."
;;   (interactive)
;;   (fpc-change-keywords 'upcase-word))

;; (defun fpc-capitalize-keywords ()
;;   "Capitalize all Pascal keywords in the buffer."
;;   (interactive)
;;   (fpc-change-keywords 'capitalize-word))

;; ;; Change the keywords according to argument.
;; (defun fpc-change-keywords (change-word)
;;   (save-excursion
;;     (let ((keyword-re (concat "\\<\\("
;; 			      (mapconcat 'identity fpc-keywords "\\|")
;; 			      "\\)\\>")))
;;       (goto-char (point-min))
;;       (while (re-search-forward keyword-re nil t)
;; 	(funcall change-word -1)))))

;;;
;;; Other functions
;;;
(defun fpc-set-auto-comments ()
  "Insert `{ case }' or `{ NAME }' on this line if appropriate.
Insert `{ case }' if there is an `end' on the line which
ends a case block.  Insert `{ NAME }' if there is an `end'
on the line which ends a function or procedure named NAME."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (looking-at "\\<end;")
	         (not (save-excursion
		            (end-of-line)
		            (search-backward "{" (point-at-bol) t))))
	    (let ((type (car (fpc-calculate-indent))))
	      (if (eq type 'declaration)
	          ()
	        (if (eq type 'case)
		        ;; This is a case block
		        (progn
		          (end-of-line)
		          (delete-horizontal-space)
		          (insert " { case }"))
	          (let ((nest 1))
		        ;; Check if this is the end of a function
		        (save-excursion
		          (while (not (or (looking-at fpc-defun-re) (bobp)))
		            (backward-sexp 1)
		            (cond ((looking-at fpc-beg-block-re)
			               (setq nest (1- nest)))
			              ((looking-at fpc-end-block-re)
			               (setq nest (1+ nest)))))
		          (if (bobp)
		              (setq nest 1)))
		        (if (zerop nest)
		            (progn
		              (end-of-line)
		              (delete-horizontal-space)
		              (insert " { ")
		              (let (b e)
			            (save-excursion
			              (setq b (progn (fpc-beg-of-defun)
					                     (skip-chars-forward "^ \t")
					                     (skip-chars-forward " \t")
					                     (point))
				                e (progn (skip-chars-forward "a-zA-Z0-9_")
					                     (point))))
			            (insert-buffer-substring (current-buffer) b e))
		              (insert " }"))))))))))

;;;
;;; Indentation
;;;
(defconst fpc-indent-alist
  '((block . (+ fpc--extra-indent fpc-indent-level))
    (case . (+ fpc--extra-indent fpc-case-indent))
    (caseblock . fpc--extra-indent) (cpp . 0)
    (declaration . (+ fpc--extra-indent fpc-indent-level))
    (paramlist . (fpc-indent-paramlist t))
    (comment . (fpc-indent-comment))
    (defun . fpc--extra-indent) (contexp . fpc--extra-indent)
    (unknown . fpc--extra-indent) (string . 0) (progbeg . 0)))

(defun fpc-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (fpc-calculate-indent))
	     (type (car indent-str)))
    (cond ((and (eq type 'paramlist)
		        (or (memq 'all fpc-auto-lineup)
		            (memq 'paramlist fpc-auto-lineup)))
	       (fpc-indent-paramlist)
	       (fpc-indent-paramlist))
	      ((and (eq type 'declaration)
		        (or (memq 'all fpc-auto-lineup)
		            (memq 'declaration  fpc-auto-lineup)))
	       (fpc-indent-declaration))
	      ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
		        (or (memq 'all fpc-auto-lineup)
		            (memq 'case fpc-auto-lineup)))
	       (fpc-indent-case)))
    (if (looking-at "[ \t]+$")
	    (skip-chars-forward " \t"))))

(defun fpc-indent-line ()
  "Indent current line as a Pascal statement."
  (let* ((indent-str (fpc-calculate-indent))
	     (type (car indent-str))
	     (fpc--extra-indent (car (cdr indent-str))))
    ;; Labels should not be indented.
    (if (and (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
	         (not (eq type 'declaration)))
	    (search-forward ":" nil t))
    (delete-horizontal-space)
    (cond (; Some things should not be indented
	       (or (and (eq type 'declaration) (looking-at fpc-declaration-re))
	           (eq type 'cpp))
	       ())
	      (; Other things should have no extra indent
	       (looking-at fpc-noindent-re)
	       (indent-to fpc--extra-indent))
	      (; Nested functions should be indented
	       (looking-at fpc-defun-re)
	       (if (and fpc-indent-nested-functions
		            (eq type 'defun))
	           (indent-to (+ fpc--extra-indent fpc-indent-level))
	         (indent-to fpc--extra-indent)))
	      (; But most lines are treated this way
	       (indent-to (eval (cdr (assoc type fpc-indent-alist))))
	       ))))

(defun fpc-calculate-indent ()
  "Calculate the indent of the current Pascal line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	       (oldpos (point))
	       (state (save-excursion (parse-partial-sexp (point-min) (point))))
	       (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	       (elsed (looking-at "[ \t]*else\\>")) (funccnt 0)
	       (did-func (looking-at "[ \t]*\\(procedure\\|function\\)\\>"))
	       (type (catch 'nesting
		           ;; Check if inside a string, comment or parenthesis
		           (cond ((nth 3 state) (throw 'nesting 'string))
			             ((nth 4 state) (throw 'nesting 'comment))
			             ((> (car state) 0)
			              (goto-char (scan-lists (point) -1 (car state)))
			              (setq par (1+ (current-column))))
			             ((save-excursion (beginning-of-line)
					                      (eq (following-char) ?#))
			              (throw 'nesting 'cpp)))
		           ;; Loop until correct indent is found
		           (while t
		             (backward-sexp 1)
		             (cond (;--Escape from case statements
			                (and (looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				                 (not complete)
				                 (save-excursion (skip-chars-backward " \t")
						                         (bolp))
				                 (= (save-excursion
				                      (end-of-line) (backward-sexp) (point))
				                    (point))
				                 (> (save-excursion (goto-char oldpos)
						                            (beginning-of-line)
						                            (point))
				                    (point)))
			                (throw 'nesting 'caseblock))
			               (;--Beginning of program
			                (looking-at fpc-progbeg-re)
			                (throw 'nesting 'progbeg))
			               (;--No known statements
			                (bobp)
			                (throw 'nesting 'progbeg))
			               (;--Nest block outwards
			                (looking-at fpc-beg-block-re)
			                (if (= nest 0)
				                (cond ((looking-at "case\\>")
				                       (throw 'nesting 'case))
				                      ((looking-at "record\\>")
				                       (throw 'nesting 'declaration))
				                      (t (throw 'nesting 'block)))
			                  (if (and (looking-at "record\\>") (= nest 1))
				                  (setq funccnt (1- funccnt)))
			                  (setq nest (1- nest))))
			               (;--Nest block inwards
			                (looking-at fpc-end-block-re)
			                (if (and (looking-at "end\\s ")
				                     elsed (not complete))
				                (throw 'nesting 'block))
			                (if (= nest 0)
				                (setq funccnt (1+ funccnt)))
			                (setq complete t
				                  nest (1+ nest)))
			               (;--Defun (or parameter list)
			                (and (looking-at fpc-defun-re)
				                 (progn (setq funccnt (1- funccnt)
					                          did-func t)
					                    (or (bolp) (< funccnt 0))))
			                ;; Prevent searching whole buffer
			                (if (and (bolp) (>= funccnt 0))
				                (throw 'nesting 'progbeg))
			                (if (= 0 par)
				                (throw 'nesting 'defun)
			                  (setq par 0)
			                  (let ((n 0))
				                (while (re-search-forward
					                    "\\(\\<record\\>\\)\\|\\<end\\>"
					                    oldpos t)
				                  (if (match-end 1)
				                      (setq n (1+ n)) (setq n (1- n))))
				                (if (> n 0)
				                    (throw 'nesting 'declaration)
				                  (throw 'nesting 'paramlist)))))
			               (;--Declaration part
			                (and (looking-at fpc-declaration-re)
				                 (not did-func)
				                 (= funccnt 0))
			                (if (save-excursion
				                  (goto-char oldpos)
				                  (forward-line -1)
				                  (looking-at "^[ \t]*$"))
				                (throw 'nesting 'unknown)
			                  (throw 'nesting 'declaration)))
			               (;--If, else or while statement
			                (and (not complete)
				                 (looking-at fpc-sub-block-re))
			                (throw 'nesting 'block))
			               (;--Found complete statement
			                (save-excursion (forward-sexp 1)
					                        (= (following-char) ?\;))
			                (setq complete t))
			               )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis
	      (list 'contexp par)
	    (list type (fpc-indent-level))))))

(defun fpc-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
	    (search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*record\\>")
	      (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun fpc-indent-comment ()
  "Return indent for current comment."
  (save-excursion
    (re-search-backward "\\((\\*\\)\\|{" nil t)
    (if (match-beginning 1)
	    (1+ (current-column))
      (current-column))))

(defun fpc-indent-case ()
  "Indent within case statements."
  (let ((savepos (point-marker))
	    (end (prog2
		         (end-of-line)
		         (point-marker)
	           (re-search-backward "\\<case\\>" nil t)))
	    (beg (point))
	    (fpc--extra-indent 0))
    ;; Get right indent
    (while (< (point) end)
      (if (re-search-forward
	       "^[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	       (marker-position end) 'move)
	      (forward-char -1))
      (if (< (point) end)
	      (progn
	        (delete-horizontal-space)
	        (if (> (current-column) fpc--extra-indent)
		        (setq fpc--extra-indent (current-column)))
	        (fpc-end-of-statement))))
    (goto-char beg)
    ;; Indent all case statements
    (while (< (point) end)
      (if (re-search-forward
	       "^[ \t]*[^][ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	       (marker-position end) 'move)
	      (forward-char -1))
      (indent-to (1+ fpc--extra-indent))
      (if (/= (following-char) ?:)
	      ()
	    (forward-char 1)
	    (delete-horizontal-space)
	    (insert " "))
      (fpc-end-of-statement))
    (goto-char savepos)))

(defun fpc-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional arg is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point))
	       (stpos (progn (goto-char (scan-lists (point) -1 1)) (point)))
	       (stcol (1+ (current-column)))
	       (edpos (progn (fpc-declaration-end)
			             (search-backward ")" (point-at-bol) t)
			             (point)))
	       (usevar (re-search-backward "\\<var\\>" stpos t)))
      (if arg (progn
		        ;; If arg, just return indent
		        (goto-char oldpos)
		        (beginning-of-line)
		        (if (or (not usevar) (looking-at "[ \t]*var\\>"))
		            stcol (+ 4 stcol)))
	    (goto-char stpos)
	    (forward-char 1)
	    (delete-horizontal-space)
	    (if (and usevar (not (looking-at "var\\>")))
	        (indent-to (+ 4 stcol)))
	    (fpc-indent-declaration nil stpos edpos)))))

(defun fpc-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s or `='s."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (fpc-declaration-beg)))
	    ()
      (let ((lineup (if (or (looking-at "\\<var\\>\\|\\<record\\>") arg start)
			            ":" "="))
	        (stpos (if start start
		             (forward-word 2) (backward-word 1) (point)))
	        (edpos (set-marker (make-marker)
			                   (if end end
				                 (max (progn (fpc-declaration-end)
					                         (point))
				                      pos))))
	        fpc--extra-indent)

	    (goto-char stpos)
	    ;; Indent lines in record block
	    (if arg
	        (while (<= (point) edpos)
	          (beginning-of-line)
	          (delete-horizontal-space)
	          (if (looking-at "end\\>")
		          (indent-to arg)
		        (indent-to (+ arg fpc-indent-level)))
	          (forward-line 1)))

	    ;; Do lineup
	    (setq fpc--extra-indent (fpc-get-lineup-indent stpos edpos lineup))
	    (goto-char stpos)
	    (while (and (<= (point) edpos) (not (eobp)))
	      (if (search-forward lineup (point-at-eol) 'move)
	          (forward-char -1))
	      (delete-horizontal-space)
	      (indent-to fpc--extra-indent)
	      (if (not (looking-at lineup))
	          (forward-line 1) ; No more indent if there is no : or =
	        (forward-char 1)
	        (delete-horizontal-space)
	        (insert " ")
	        ;; Indent record block
	        (if (looking-at "record\\>")
		        (fpc-indent-declaration (current-column)))
	        (forward-line 1)))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char pos))))

                                        ;  "Return the indent level that will line up several lines within the region
                                        ;from b to e nicely. The lineup string is str."
(defun fpc-get-lineup-indent (b e str)
  (save-excursion
    (let ((fpc--extra-indent 0)
	      (reg (concat str "\\|\\(\\<record\\>\\)\\|" fpc-defun-re)))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	    (and (re-search-forward reg (min e (point-at-eol 2)) 'move)
	         (cond ((match-beginning 1)
		            ;; Skip record blocks
		            (fpc-declaration-end))
		           ((match-beginning 2)
		            ;; We have entered a new procedure.  Exit.
		            (goto-char e))
		           (t
		            (goto-char (match-beginning 0))
		            (skip-chars-backward " \t")
		            (if (> (current-column) fpc--extra-indent)
			            (setq fpc--extra-indent (current-column)))
		            (goto-char (match-end 0))
		            (end-of-line)
		            ))))
      ;; In case no lineup was found
      (if (> fpc--extra-indent 0)
	      (1+ fpc--extra-indent)
	    ;; No lineup-string found
	    (goto-char b)
	    (end-of-line)
	    (skip-chars-backward " \t")
	    (1+ (current-column))))))

;;;
;;; Completion

(defun fpc-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	    (if (or (> (1+ diff) (length str1))
		        (> (1+ diff) (length str2)))
	        (throw 'done diff))
	    (or (equal (aref str1 diff) (aref str2 diff))
	        (throw 'done diff))
	    (setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun fpc-func-completion (type fpc-str)
  ;; Build regular expression for function/procedure names
  (save-excursion
    (if (string= fpc-str "")
        (setq fpc-str "[a-zA-Z_]"))
    (let ((fpc-str (concat (cond
                            ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
                            ((eq type 'function) "\\<\\(function\\)\\s +")
                            (t "\\<\\(function\\|procedure\\)\\s +"))
                           "\\<\\(" fpc-str "[a-zA-Z0-9_.]*\\)\\>"))
          (fpc-all ())
          match)

      (if (not (looking-at "\\<\\(function\\|procedure\\)\\>"))
          (re-search-backward "\\<\\(function\\|procedure\\)\\>" nil t))
      (forward-char 1)

      ;; Search through all reachable functions
      (while (fpc-beg-of-defun)
        (if (re-search-forward fpc-str (point-at-eol) t)
            (progn (setq match (buffer-substring (match-beginning 2)
                                                 (match-end 2)))
                   (push match fpc-all)))
        (goto-char (match-beginning 0)))

      fpc-all)))

(defun fpc-get-completion-decl (fpc-str)
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurrence to `all'
  (let ((end (save-excursion (fpc-declaration-end)
			                 (point)))
        (fpc-all ())
	    match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (point-at-eol) t)
	      ;; Traverse current line
	      (while (and (re-search-backward
		               (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|"
			                   fpc-symbol-re)
		               (point-at-bol) t)
		              (not (match-end 1)))
	        (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	        (if (string-match (concat "\\<" fpc-str) match)
                (push match fpc-all))))
      (if (re-search-forward "\\<record\\>" (point-at-eol) t)
	      (fpc-declaration-end)
	    (forward-line 1)))

    fpc-all))

(defun fpc-type-completion (fpc-str)
  "Calculate all possible completions for types."
  (let ((start (point))
        (fpc-all ())
	    goon)
    ;; Search for all reachable type declarations
    (while (or (fpc-beg-of-defun)
	           (setq goon (not goon)))
      (save-excursion
	    (if (and (< start (prog1 (save-excursion (fpc-end-of-defun)
						                         (point))
			                (forward-char 1)))
		         (re-search-forward
		          "\\<type\\>\\|\\<\\(begin\\|function\\|procedure\\)\\>"
		          start t)
		         (not (match-end 1)))
	        ;; Check current type declaration
            (setq fpc-all
                  (nconc (fpc-get-completion-decl fpc-str)
                         fpc-all)))))

    fpc-all))

(defun fpc-var-completion (prefix)
  "Calculate all possible completions for variables (or constants)."
  (save-excursion
    (let ((start (point))
          (fpc-all ())
          goon twice)
      ;; Search for all reachable var declarations
      (while (or (fpc-beg-of-defun)
                 (setq goon (not goon)))
        (save-excursion
          (if (> start (prog1 (save-excursion (fpc-end-of-defun)
                                              (point))))
              ()                        ; Declarations not reachable
            (if (search-forward "(" (point-at-eol) t)
                ;; Check parameterlist
                ;; FIXME: fpc-get-completion-decl doesn't understand
                ;; the var declarations in parameter lists :-(
                (setq fpc-all
                      (nconc (fpc-get-completion-decl prefix)
                             fpc-all)))
            (setq twice 2)
            (while (>= (setq twice (1- twice)) 0)
              (cond
               ((and (re-search-forward
                      (concat "\\<\\(var\\|const\\)\\>\\|"
                              "\\<\\(begin\\|function\\|procedure\\)\\>")
                      start t)
                     (not (match-end 2)))
                ;; Check var/const declarations
                (setq fpc-all
                      (nconc (fpc-get-completion-decl prefix)
                             fpc-all)))
               ((match-end 2)
                (setq twice 0)))))))
      fpc-all)))


(defun fpc-keyword-completion (keyword-list fpc-str)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (let ((fpc-all ()))
    (dolist (s keyword-list)
      (if (string-match (concat "\\<" fpc-str) s)
          (push s fpc-all)))
    fpc-all))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defvar fpc-completion-cache nil)

(defun fpc-completion (fpc-str fpc-pred fpc-flag)
  (let ((all (car fpc-completion-cache)))
    ;; Check the cache's freshness.
    (unless (and fpc-completion-cache
                 (string-prefix-p (nth 1 fpc-completion-cache) fpc-str)
                 (eq (current-buffer) (nth 2 fpc-completion-cache))
                 (eq (field-beginning) (nth 3 fpc-completion-cache)))
      (let ((state (car (fpc-calculate-indent))))
        (setq all
              ;; Determine what should be completed
              (cond
               (              ;--Within a declaration or parameterlist
                (or (eq state 'declaration) (eq state 'paramlist)
                    (and (eq state 'defun)
                         (save-excursion
                           (re-search-backward ")[ \t]*:" (point-at-bol) t))))
                (if (or (eq state 'paramlist) (eq state 'defun))
                    (fpc-beg-of-defun))
                (nconc
                 (fpc-type-completion fpc-str)
                 (fpc-keyword-completion fpc-type-keywords fpc-str)))
               (                        ;--Starting a new statement
                (and (not (eq state 'contexp))
                     (save-excursion
                       (skip-chars-backward "a-zA-Z0-9_.")
                       (backward-sexp 1)
                       (or (looking-at fpc-nosemi-re)
                           (progn
                             (forward-sexp 1)
                             (looking-at "\\s *\\(;\\|:[^=]\\)")))))
                (nconc
                 (fpc-var-completion fpc-str)
                 (fpc-func-completion 'procedure fpc-str)
                 (fpc-keyword-completion fpc-start-keywords fpc-str)))
               (t                       ;--Anywhere else
                (nconc
                 (fpc-var-completion fpc-str)
                 (fpc-func-completion 'function fpc-str)
                 (fpc-keyword-completion fpc-separator-keywords
                                         fpc-str)))))

        (setq fpc-completion-cache
              (list all fpc-str (current-buffer) (field-beginning)))))

    ;; Now we have built a list of all matches. Give response to caller
    (complete-with-action fpc-flag all fpc-str fpc-pred)))

(defvar fpc-last-word-numb 0)
(defvar fpc-last-word-shown nil)
(defvar fpc-last-completions nil)

(defun fpc-completions-at-point ()
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	     (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point))))
    (when (> e b)
      (list b e #'fpc-completion))))

(define-obsolete-function-alias 'fpc-complete-word
  'completion-at-point "24.1")

(define-obsolete-function-alias 'fpc-show-completions
  'completion-help-at-point "24.1")


(defun fpc-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			            (skip-chars-backward " \t")
			            (skip-chars-backward "a-zA-Z0-9_")
			            (point))
		              (progn
			            (skip-chars-forward "a-zA-Z0-9_")
			            (point)))))

(defun fpc-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun fpc-comp-defun (fpc-str fpc-pred fpc-flag)
  (save-excursion
    (let ((fpc-all nil))

      ;; Build regular expression for functions
      (let ((fpc-str (fpc-build-defun-re (if (string= fpc-str "")
                                             "[a-zA-Z_]"
                                           fpc-str))))
        (goto-char (point-min))

        ;; Build a list of all possible completions
        (while (re-search-forward fpc-str nil t)
          (push (match-string 2) fpc-all)))

      ;; Now we have built a list of all matches. Give response to caller
      (complete-with-action fpc-flag fpc-all fpc-str fpc-pred))))

(defun fpc-goto-defun ()
  "Move to specified Pascal function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (fpc-get-default-symbol))
	     (default (if (fpc-comp-defun default nil 'lambda)
		              default ""))
	     (label
          ;; Do completion with default.
          (completing-read (if (not (string= default ""))
                               (concat "Label (default " default "): ")
                             "Label: ")
                           ;; Complete with the defuns found in the
                           ;; current-buffer.
                           (let ((buf (current-buffer)))
                             (lambda (s p a)
                               (with-current-buffer buf
                                 (fpc-comp-defun s p a))))
                           nil t "")))
    ;; If there was no response on prompt, use default value.
    (if (string= label "")
	    (setq label default))
    ;; Goto right place in buffer if label is not an empty string.
    (or (string= label "")
	    (progn
	      (goto-char (point-min))
	      (re-search-forward (fpc-build-defun-re label t))
	      (beginning-of-line)))))

;;;
;;; Fpc-outline-mode
;;;
(defvar fpc-outline-map
  (let ((map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-name)
        (set-keymap-name fpc-outline-map 'fpc-outline-map))
    (define-key map "\M-\C-a"  'fpc-outline-prev-defun)
    (define-key map "\M-\C-e"  'fpc-outline-next-defun)
    (define-key map "\C-c\C-d" 'fpc-outline-goto-defun)
    (define-key map "\C-c\C-s" 'fpc-show-all)
    (define-key map "\C-c\C-h" 'fpc-hide-other-defuns)
    map)
  "Keymap used in Pascal Outline mode.")

(define-obsolete-function-alias 'fpc-outline 'fpc-outline-mode "22.1")
(define-minor-mode fpc-outline-mode
  "Outline-line minor mode for Pascal mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When enabled, portions of the text being edited may be made
invisible. \\<fpc-outline-map>

Pascal Outline mode provides some additional commands.

\\[fpc-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[fpc-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[fpc-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[fpc-show-all]\t- Show the whole buffer.
\\[fpc-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[fpc-outline]\t- Leave fpc-outline-mode."
  :init-value nil :lighter " Outl" :keymap fpc-outline-map
  (add-to-invisibility-spec '(fpc-mode . t))
  (unless fpc-outline-mode
    (fpc-show-all)))

(defun fpc-outline-change (b e hide)
  (when (> e b)
    ;; We could try and optimize this in the case where the region is
    ;; already hidden.  But I'm not sure it's worth the trouble.
    (remove-overlays b e 'invisible 'fpc-mode)
    (when hide
      (let ((ol (make-overlay b e nil t nil)))
        (overlay-put ol 'invisible 'fpc-mode)
        (overlay-put ol 'evaporate t)))))

(defun fpc-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (fpc-outline-change (point-min) (point-max) nil))

(defun fpc-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
			              (fpc-beg-of-defun))
		              (line-beginning-position)))
	      (end (progn (fpc-end-of-defun)
		              (backward-sexp 1)
                      (line-beginning-position 2)))
	      (opoint (point-min)))
      ;; BEG at BOL.
      ;; OPOINT at EOL.
      ;; END at BOL.
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^[ \t]*\\(function\\|procedure\\)\\>"
                                beg 'move)
	    (fpc-outline-change opoint (line-end-position 0) t)
	    (setq opoint (line-end-position))
	    ;; Functions may be nested
	    (if (> (progn (fpc-end-of-defun) (point)) beg)
	        (goto-char opoint)))
      (if (> beg opoint)
	      (fpc-outline-change opoint (1- beg) t))

      ;; Show current function
      (fpc-outline-change (1- beg) end nil)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" end 'move)
	    (setq opoint (line-end-position))
	    (fpc-end-of-defun)
	    (fpc-outline-change opoint (line-end-position) t))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" nil 'move)
	    (fpc-outline-change opoint (line-end-position 0) t)
	    (setq opoint (line-end-position))
	    (fpc-end-of-defun))
      (fpc-outline-change opoint (point-max) t)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
	      (progn
	        (goto-char beg)
	        (fpc-end-of-defun)
	        (backward-sexp 1)
	        (fpc-outline-change (line-end-position) (point-max) t))))))

(defun fpc-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (fpc-end-of-defun)
  (fpc-hide-other-defuns))

(defun fpc-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (fpc-beg-of-defun)
  (fpc-hide-other-defuns))

(defun fpc-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (fpc-goto-defun)
  (fpc-hide-other-defuns))

(provide 'fpc-mode)

;;; fpc-mode.el ends here
