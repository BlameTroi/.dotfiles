;;; fpc-mode.el --- major mode for editing fpc pascal source in Emacs
;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Espen Skoglund (espensk@stud.cs.uit.no)
;; Keywords: languages


;; Modifications for use with Delphi by Christian Sperr
;;
;; The modifications are not part of the original Gnu Emacs
;;
;; Further modificaitons by Troy Brumley for Free Pascal, which is
;; slightly different from Delphi.


;; This file is (was originally) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Throughout the following original commentary I've left 'Pascal'
;;; in the text even though the variables and function names use
;;; 'FPC'.

;;; USAGE
;;; =====

;;; Emacs should enter Pascal mode when you find a Pascal source file.
;;; When you have entered Pascal mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;; C-h f <Name of function you want described>

;;; If you want to customize Pascal mode to fit you better, you may add
;;; these lines (the values of the variables presented here are the defaults):
;;;
;;; ;; User customization for Pascal mode
;;; (setq pascal-indent-level       3
;;;       pascal-case-indent        2
;;;       pascal-auto-newline       nil
;;;       pascal-tab-always-indent  t
;;;       pascal-auto-endcomments   t
;;;       pascal-auto-lineup        '(all)
;;;       pascal-toggle-completions nil
;;;       pascal-type-keywords      '("array" "file" "packed" "char" 
;;; 				      "integer" "real" "string" "record")
;;;       pascal-start-keywords     '("begin" "end" "function" "procedure"
;;; 				      "repeat" "until" "while" "read" "readln"
;;; 				      "reset" "rewrite" "write" "writeln")
;;;       pascal-separator-keywords '("downto" "else" "mod" "div" "then"))

;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; As far as I know, there are no bugs in the current version of this
;;; package.  This may not be true however, since I never use this mode
;;; myself and therefore would never notice them anyway.   If you do
;;; find any bugs, you may submit them to: espensk@stud.cs.uit.no
;;; as well as to bug-gnu-emacs@prep.ai.mit.edu.

;;; Code:

;; So that dynamic Abbrev ignores qualification prefixes 

(setq dabbrev-abbrev-skip-leading-regexp "\\(\\sw+\\.\\)+" )

;; to use make-regexp for regular expressions

;(eval-when-compile (load-file "make-regexp.el"))

(defvar fpc-mode-abbrev-table nil
  "Abbrev table in use in FPC-mode buffers.")
(define-abbrev-table 'fpc-mode-abbrev-table ())

(defvar fpc-mode-map ()
  "Keymap used in FPC mode.")
(if fpc-mode-map
    ()
  (setq fpc-mode-map (make-sparse-keymap))
  (define-key fpc-mode-map ";"        'electric-fpc-semi-or-dot)
  (define-key fpc-mode-map "."        'electric-fpc-semi-or-dot)
  (define-key fpc-mode-map ":"        'electric-fpc-colon)
  (define-key fpc-mode-map "="        'electric-fpc-equal)
;  (define-key fpc-mode-map "#"        'electric-fpc-hash)
  (define-key fpc-mode-map "\r"       'electric-fpc-terminate-line)
  (define-key fpc-mode-map "\t"       'electric-fpc-tab)
  (define-key fpc-mode-map "\M-\t"    'fpc-complete-word)
  (define-key fpc-mode-map "\M-?"     'fpc-show-completions)
  (define-key fpc-mode-map "\177"     'backward-delete-char-untabify)
  (define-key fpc-mode-map "\M-\C-h"  'fpc-mark-defun)
  (define-key fpc-mode-map "\C-c\C-b" 'fpc-insert-block)
  (define-key fpc-mode-map "\M-*"     'fpc-star-comment)
  (define-key fpc-mode-map "\C-c\C-c" 'fpc-comment-area)
  (define-key fpc-mode-map "\C-c\C-u" 'fpc-uncomment-area)
  (define-key fpc-mode-map "\M-\C-a"  'fpc-beg-of-defun)
  (define-key fpc-mode-map "\M-\C-e"  'fpc-end-of-defun)
  (define-key fpc-mode-map "\C-c\C-d" 'fpc-goto-defun)
  (define-key fpc-mode-map "\C-c\C-o" 'fpc-outline)
;;; A command to change the whole buffer won't be used terribly
;;; often, so no need for a key binding.
;  (define-key fpc-mode-map "\C-cd"    'fpc-downcase-keywords)
;  (define-key fpc-mode-map "\C-cu"    'fpc-upcase-keywords)
;  (define-key fpc-mode-map "\C-cc"    'fpc-capitalize-keywords)
  )

(defvar fpc-imenu-generic-expression
  '("^[ \t]*\\(function\\|procedure\\)[ \t\n]+\\([a-zA-Z0-9_.:]+\\)" . (2))
  "Imenu expression for fpc-mode.  See `imenu-generic-expression'.")
  
(defvar fpc-keywords
  '("and" "array" "as" "asm" "begin" "case" "class" "const" "constructor" 
    "destructor" "div" "do" "downto" "else" "end" "except" "exports" 
    "file" "finalization" "finally" "for" "function" "goto" "if" 
    "implementation" "in" "inherited" "initialization" "inline" "interface"
    "is" "label" "library" "mod" "nil" "not" "object" "of" "or" "packed" 
    "procedure" "program" "property" "raise" "record" "repeat" "set" 
    "shl" "shr" "string" "then" "threadvar" "to" "try" "type" "unit" "until"
    "uses" "var" "while" "with" "xor" 
    ;; standard directives
    "absolute" "abstract" "assembler" "automated" "cdecl" "default" "dispid"
    "dynamic" "export" "external" "far" "forward" "index" "message" "name"
    "near" "nodefault" "override" "pascal" "private" "protected" "public"
    "published" "read" "register" "resident" "stdcall" "stored" "virtual"
    "write" ))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst fpc-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst fpc-beg-block-re   "\\<\\(begin\\|case\\|class\\|interface\\|object\\|record\\|repeat\\|try\\)\\>")
(defconst fpc-no-block-re "\\(\\<class *\\(function\\|procedure\\|(.*)\\)?\\|\\(\\<procedure\\|\\<function\\) +of object\\);")
(defconst fpc-end-block-re   "\\<\\(end\\.?\\|until\\)\\>")
(defconst fpc-declaration-re "\\<\\(const\\|label\\|type\\|uses\\|var\\)\\>")
(defconst fpc-defun-re      "\\<\\(function\\|procedure\\|program\\|unit\\|library\\|constructor\\|destructor\\|property\\)\\>")
(defconst fpc-sub-block-re   "\\<\\(if\\|else\\|except\\|for\\|on\\|while\\|with\\)\\>")
(defconst fpc-noindent-re    "\\<\\(begin\\|end\\|until\\|else\\|except\\|finally\\|initialization\\|finalization\\|interface\\|implementation\\|program\\|library\\)\\>")
(defconst fpc-class-prot-re "\\<\\(private\\|protected\\|public\\|published\\)\\>")
(defconst fpc-nosemi-re      "\\<\\(begin\\|repeat\\|then\\|do\\|else\\)\\>")
(defconst fpc-autoindent-lines-re
  "\\<\\(label\\|var\\|type\\|const\\|until\\|end\\|except\\|finally\\|begin\\|repeat\\|else\\|interface\\|implementation\\|initialization\\|finalization\\|uses\\private\\|public\\|published\\|protected\\)\\>")
(defconst fpc-sections-re "\\<\\(interface\\|implementation\\|initialization\\|finalization\\|end\\.\\)\\>")

(defconst fpc-directives-re "\\<\\(virtual\\|dynamic\\|message\\|register\\|pascal\\|cdecl\\|stdcall\\|abstract\\|override\\)\\>")

(defconst fpc-caselabel-re
   "^[ \t]*[^ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:")


;;; Strings used to mark beginning and end of excluded text
(defconst fpc-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst fpc-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

(defvar fpc-mode-syntax-table nil
  "Syntax table in use in fpc-mode buffers.")

(if fpc-mode-syntax-table
    ()
  (setq fpc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "."   fpc-mode-syntax-table)
  (modify-syntax-entry ?\( "()1"  fpc-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4"  fpc-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" fpc-mode-syntax-table)
  (modify-syntax-entry ?{ "<"    fpc-mode-syntax-table)
  (modify-syntax-entry ?} ">"    fpc-mode-syntax-table)
  (modify-syntax-entry ?+ "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?- "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?= "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?% "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?< "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?> "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?& "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?| "."    fpc-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    fpc-mode-syntax-table)
  (modify-syntax-entry ?. "_"    fpc-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  fpc-mode-syntax-table)
  (modify-syntax-entry ?/' ". 124b" fpc-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" fpc-mode-syntax-table)
)

(defconst fpc-font-lock-keywords
  (list
   '("^[ \t]*\\(\\(class\\)?[ \t]*\\(\\(con\\|de\\)structor\\|function\\|pro\\(cedure\\|gram\\)\\|unit\\)\\>\\)[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
     (1 font-lock-keyword-face) (6 font-lock-function-name-face nil t))
;   ("type" "const" "real" "integer" "char" "boolean" "var"
;    "record" "array" "file")
   (cons (concat "\\<\\(array\\|boolean\\|c\\(har\\|onst\\|lass\\)\\|file\\|"
		 "integer\\|packed\\|re\\(al\\|cord\\)\\|set\\|string\\|type\\|var\\)\\>")
	 'font-lock-type-face)
;;   '("\\<\\(label\\|external\\|forward\\)\\>" . font-lock-reference-face)
;;   '("\\<\\([0-9]+\\)[ \t]*:" 1 font-lock-reference-face)
;   ("of" "to" "for" "if" "then" "else" "case" "while"
;    "do" "until" "and" "or" "not" "in" "with" "repeat" "begin" "end")
   (concat "\\<\\("
	   "a\\(bsolute\\|bstract\\|nd\\|s\\|sm\\|ssembler\\|utomated\\)\\|"
	   "begin\\|c\\(ase\\|decl\\)\\|"
	   "d\\(efault\\|ispid\\|iv\\|o\\(\\|wnto\\)\\|ynamic\\)\\|"
	   "e\\(lse\\|nd\\.?\\|xcept\\|xports?\\|xternal\\)\\|"
	   "far\\|fin\\(alization\\|ally\\)\\|for\\(\\|ward\\)\\|"
	   "i[fns]\\|implementation\\|"
	   "in\\(dex\\|herited\\|itialization\\|line\\|terface\\)\\|"
	   "library\\|message\\|mod\\|name\\|near\\|no\\(t\\|default\\)\\|"
	   "o[fnr]\\|object\\|override\\|pascal\\|"
	   "pr\\(ivate\\|operty\\|otected\\)\\|publi\\(c\\|shed\\)\\|"
	   "r\\(aise\\|ead\\|egister\\|epeat\\|esident\\)\\|"
	   "sh[lr]\\|stdcall\\|stored\\|t\\(hen\\|hreadvar\\|o\\|ry\\)\\|"
	   "un\\(it\\|til\\)\\|uses\\|virtual\\|w\\(hile\\|ith\\|rite\\)\\|xor"
	   "\\)\\>")
   '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-reference-face nil t)))
  "Additional expressions to highlight in Pascal mode.")

(defvar fpc-indent-level 2
  "*Indentation of Pascal statements with respect to containing block.")

(defvar fpc-case-indent 2
  "*Indentation for case statements.")

(defvar fpc-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctation mark
after an end.")

(defvar fpc-tab-always-indent t
  "*Non-nil means TAB in Pascal mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar fpc-auto-endcomments t
  "*Non-nil means a comment { ... } is set after the ends which ends cases and
functions. The name of the function or case will be set between the braces.")

(defvar fpc-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively. The word 'all' will do all lineups. '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups.")

(defvar fpc-toggle-completions nil
  "*Non-nil means \\<fpc-mode-map>\\[fpc-complete-word] should try all possible completions one by one.
Repeated use of \\[fpc-complete-word] will show you all of them.
Normally, when there is more than one possible completion,
it displays a list of all possible completions.")

(defvar fpc-type-keywords
  '("array" "file" "packed" "char" "integer" "real" "string" "record")
  "*Keywords for types used when completing a word in a declaration or parmlist.
\(eg. integer, real, char.)  The types defined within the Pascal program
will be completed runtime, and should not be added to this list.")

(defvar fpc-start-keywords
  '("begin" "end" "function" "procedure" "repeat" "until" "while"
    "read" "readln" "reset" "rewrite" "write" "writeln")
  "*Keywords to complete when standing at the first word of a statement.
\(eg. begin, repeat, until, readln.)
The procedures and variables defined within the Pascal program
will be completed runtime and should not be added to this list.")

(defvar fpc-separator-keywords
  '("downto" "else" "mod" "div" "then")
  "*Keywords to complete when NOT standing at the first word of a statement.
\(eg. downto, else, mod, then.) 
Variables and function names defined within the
Pascal program are completed runtime and should not be added to this list.")

;;;
;;;  Macros
;;;

(defsubst fpc-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst fpc-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))
  
(defsubst fpc-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (fpc-get-beg-of-line) (point)))))

(defsubst fpc-within-comment (startpos)
  (save-excursion
    (nth 4 (parse-partial-sexp startpos (point)))))

; sets the point to the end of declaration -- not quite true for delphi
(defun fpc-declaration-end (&optional arg)
  (let* ((inclass (if arg arg 0))
	 (nest inclass)
	 (startpoint (point))
	 (reg (concat 
	      "\\<\\(procedure\\|function\\|constructor\\|destructor\\>\\)"
	      "\\|\\<\\(begin\\|implementation\\)\\>\\|"
	      "\\(\\<property\\>\\|"
	      "\\<published\\|public\\|protected\\|private\\|automated\\>\\)\\|"
	      "[:=]\\|\\(\\<record\\>\\|\\<class\\>\\|\\<interface\\>\\|"
	      "\\<object\\>\\)"
	      "\\|\\(\\<end\\>\\)\\|"
	      "\\<\\(var\\|const\\)\\>\\|\\<\\(type\\)\\>\\|\\()\\)\\|\\((\\)" ))
	 (found nil)
	 (paren (save-excursion
		  (nth 0 (parse-partial-sexp (point-min) (point)))))
	 (parenstart (> paren 0)))
    (while (and 
	    (not found)
	    (re-search-forward 
	     reg
	     ;(save-excursion (end-of-line 5) (point))
	     (point-max) t))
      (if (not (fpc-within-comment startpoint))
	  (cond ((match-beginning 1) 
					;procedure, function,constructor or 
					;destructor
		 (if parenstart (setq found t)
		   (setq found 
			 (and (= nest 0)
			      (save-excursion
				(forward-sexp -2)
				(forward-sexp 1)
				(not (looking-at "[ \t]*="))))
			 )))
		((match-beginning 2) 
					;begin, implementation
		 (setq found t))
	    ((match-beginning 3) 
					;property, published, public, 
					;protected, private, automated
	     (if parenstart (setq found t))
	     (if (= 0 inclass) (setq inclass (setq nest (1+ nest)))))
	    ((match-beginning 4) 
					;record, class, object, interface
	     (save-excursion
	       (forward-word -1)
	       (if (looking-at "\\(class\\|interface\\)\\((.*)\\)?[^;]")
		   (setq inclass 1))
	       (if (looking-at "class;")
		   (setq nest (1- nest)))
	       (setq nest (1+ nest))))
	    ((match-beginning 5) 
					;end
	     (setq nest (1- nest))
	     (if (< nest inclass) (setq inclass nest)))
	    ((match-beginning 6) 
					;var, const
	     (if (= 0 paren) (setq found t)))
	    ((match-beginning 7)
					;type
	     (if (<= 0 paren) (setq found t)))
	    ((match-beginning 8) 
					; )
	     (setq paren (1- paren)) 
	     (if (and parenstart (= 0 paren)) (setq found t)))
	    ((match-beginning 9)
					; (
	     (setq paren (1+ paren))))))
    (if (or (match-beginning 1) (match-beginning 2) (match-beginning 3)
	    (and (not parenstart) found)) (forward-word -1))
    (skip-chars-backward " \t\n")
;;    (if (looking-at "\\<function\\|procedure\\|constructor\\|destructor\\>")
;;	(forward-line -1))
    ))


;should set point to the beginning of a declaration
(defun fpc-declaration-beg ()
  (let ((nest 0)
	(found nil)
	(paren (save-excursion
		 (nth 0 (parse-partial-sexp (point-min) (point))))))
    (while (and (not found)
		(re-search-backward 
		 (concat "[:=]\\|"
			 "\\<\\(type\\|label\\)\\>\\|"
			 "\\(\\<record\\>\\|\\<class\\>\\|\\<interface\\>"
			 "\\|\\<object\\>\\)\\|"
			 "\\(\\<end\\>\\)\\|"
			 "\\<\\(var\\|const\\)\\>\\|\\()\\)\\|\\((\\)" )
		 (fpc-get-beg-of-line -5) t))
      (cond ((match-beginning 1) (setq found t))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((match-beginning 3) (setq nest (1+ nest)))
	    ((match-beginning 4) (if (= 0 paren) (setq found t)) )
	    ((match-beginning 5) (setq paren (1+ paren)))
	    ((match-beginning 6) (setq paren (1- paren)))))

    (= 0 paren)))


;; returns true if the point is in a section that can contain implementations
;; of functions and procedures

(defun fpc-in-implementation ()

  (save-excursion
    (let ((found nil)) 
      (while (and (not (bobp)) (not found)) 
	(if (re-search-backward "\\<\\(implementation\\|program\\|library\\)\\>" 
				(point-min) 'move )
	    (setq found 
		  (not (let ((state (save-excursion (parse-partial-sexp 
						     (point-min) (point)))))
			 (or (nth 3 state) (nth 4 state)))))))
      found)))
    

(defun fpc-defun-valid ()
;;; returns t if the subroutine at point really starts the definition of
;;; a subroutine or a program or library
  (if (looking-at "program\\|library") t
    (if (fpc-in-implementation)
	(save-excursion
	  (skip-chars-backward " \t\n")
	  (if (looking-at "=") nil
	    (let ((nest 1) (found nil))
	      (while (and (> nest 0) (not (bobp)))
		(re-search-backward "\\(=[ \t\n]*\\(class\\|object\\interface\\)\\)\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (point-min) 'move)
		(cond
		 ((bobp))
		 ((match-beginning 4) (setq nest (1+ nest)))
		 ((match-beginning 3) (setq nest (1- nest)))
		 ((and (match-beginning 1) (>= 1 nest)) 
		  (setq nest 0) (setq found t))))
	      (not found))))
      nil)))
	   
    
(defun fpc-hs-forward-sexp (&optional arg)
  "Function for use with hide-show-minor-mode for skipping blocks"
  (let ((nest 0) (spos (point)))
    (if (and arg (< arg 0))
	(progn
	  (save-excursion 
	    (forward-word -1)
	    (if (looking-at fpc-end-block-re)
		(setq nest -1)))
	  (if (= nest 0)
	      (forward-sexp -1)
	    (while (and (< nest 0) (not (bobp)))
	      (re-search-backward (concat fpc-beg-block-re "\\|"
					  fpc-end-block-re)
				  (point-min) 'move)
	      (cond
	       ((bobp))
	       ((and (match-beginning 1) 
		     (not (fpc-within-comment spos))
		     (not 
		      (looking-at fpc-no-block-re)
		      )) (setq nest (1+ nest)))
	       ((and (match-beginning 2) (not (fpc-within-comment spos)))
		(setq nest (1- nest)))))
	    (if (= nest 0)
		(goto-char (match-beginning 1)))))
      ;; Positives Argument
      (if (not (looking-at fpc-beg-block-re))
	  (forward-sexp 1)
	(forward-word 1)
	(setq nest 1)
	(while (and (> nest 0) (not (eobp)))
	  (re-search-forward (concat fpc-beg-block-re "\\|"
				     fpc-end-block-re)
			     (point-max) 'move)
	  (cond
	   ((eobp))
	   ((and (match-beginning 1) 
		 (not (fpc-within-comment spos))
		 (save-excursion (goto-char (match-beginning 1))
				 (not (looking-at fpc-no-block-re)))
		 )
	    (setq nest (1+ nest)))
	   ((and (match-beginning 2) (not (fpc-within-comment spos)))
	    (setq nest (1- nest)))))
	(if (= nest 0)
	    (goto-char (match-end 2)))))))


;;;###autoload
(defun fpc-mode ()
  "Major mode for editing Pascal code. \\<fpc-mode-map>
TAB indents for Pascal code.  Delete converts tabs to spaces as it moves back.

\\[fpc-complete-word] completes the word around current point with respect \
to position in code
\\[fpc-show-completions] shows all possible completions at this point.

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
\\[fpc-outline]\t- Enter fpc-outline-mode (see also fpc-outline).

Variables controlling indentation/edit style:

 fpc-indent-level      (default 2)
    Indentation of Pascal statements with respect to containing block.
 fpc-case-indent       (default 2)
    Indentation for case statements.
 fpc-auto-newline      (default nil)
    Non-nil means automatically newline after simcolons and the punctation mark
    after an end.
 fpc-tab-always-indent (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 fpc-auto-endcomments  (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 fpc-auto-lineup       (default t)
    List of contexts where auto lineup of :'s or ='s hould be done.

See also the user variables fpc-type-keywords, fpc-start-keywords and
fpc-separator-keywords.

Turning on Pascal mode calls the value of the variable fpc-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map fpc-mode-map)
  (setq major-mode 'fpc-mode)
  (setq mode-name "FPC")
  (setq local-abbrev-table fpc-mode-abbrev-table)
  (set-syntax-table fpc-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'fpc-indent-line)
  (setq comment-indent-function 'fpc-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start "{")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *\\|{+ *\\|// *")
  (make-local-variable 'comment-end)
  (setq comment-end "}")
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(fpc-font-lock-keywords nil t))
  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression fpc-imenu-generic-expression)
  (tempo-use-tag-list 'fpc-tempo-tags)
  (run-hooks 'fpc-mode-hook))



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
  (if (save-excursion
	(not (nth 4 (parse-partial-sexp (point-min) (point)))))
	()
	(let ((setstar nil))
	  (save-excursion
	    (forward-line -1)
	    (skip-chars-forward " \t")
	    (cond ((looking-at "\\*[ \t]+)")
		   ;; Delete region between `*' and `)' if there is 
		   ;; only whitespaces.
		   (forward-char 1)
		   (delete-horizontal-space))
		  ((and (looking-at "(\\*\\|\\*[^)]")
			(not (save-excursion
			       (search-forward "*)" 
					       (fpc-get-end-of-line) t))))
		   (setq setstar t))))
	  ;; If last line was a star comment line then this one shall be too.
	  (if (null setstar)	
	      (fpc-indent-line)
	    (insert "*  ")))))
      
(defun fpc-reindent-line ()
  "Indent line even if point is not at the beginning"
  (save-excursion
    (beginning-of-line)
    (fpc-indent-line)
    nil))

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
  "Insert `:' and do all indentions except line indent on this line."
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
  "Insert `=', and do indention if within type declaration."
  (interactive)
  (insert last-command-event)
  (if (eq (car (fpc-calculate-indent)) 'declaration)
      (let ((fpc-tab-always-indent nil))
	(fpc-indent-command))))

(defun electric-fpc-hash ()
  "Insert `#', and indent to coulmn 0 if this is a CPP directive."
  (interactive)
  (insert last-command-event)
; in borland pascal # is used for character constants
;  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
;      (save-excursion (beginning-of-line)
;		      (delete-horizontal-space)))
)

(defun electric-fpc-tab ()
  "Function called when TAB is pressed in Pascal mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (fpc-within-string)
	  ;;(and (not (bolp))
	  ;;     (save-excursion (beginning-of-line) (eq (following-char) ?#)))
	  )
      (insert "\t")
    ;; If fpc-tab-always-indent, indent the beginning of the line.
    (if fpc-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (fpc-indent-line))
      (insert "\t"))
    (fpc-indent-command)))



;;;
;;; Interactive functions
;;;
(defun fpc-insert-block ()
  "Insert Pascal begin ... end; block in the code with right indentation."
  (interactive)
  (fpc-indent-line)
  (insert "begin")
  (electric-fpc-terminate-line)
  (save-excursion
    (electric-fpc-terminate-line)
    (insert "end;")
    (beginning-of-line)
    (fpc-indent-line)
    (end-of-line)))
;; Wieso steht man nicht auf der Leerzeile ?


(defun fpc-star-comment ()
  "Insert Pascal star comment at point."
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
  "Mark the current pascal function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (fpc-end-of-defun)
  (push-mark (point))
  (fpc-beg-of-defun)
  (if (fboundp 'zmacs-activate-region)
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

(defun fpc-beg-of-defun (&optional arg)
  "Move backward to the beginning of the current function or procedure.
if arg is set then stop when indentation of a function or procedure is
less or eqal to fpc-indent-level."
  (interactive "P")
  (catch 'found
    ;(if (not (looking-at (concat "\\s \\|\\s)\\|" fpc-defun-re)))
    ;	(forward-sexp 1))
    (let ((nest 0) (endlist ()) p
	  (reg (concat fpc-beg-block-re "\\|"
		       fpc-end-block-re "\\|"
		       fpc-defun-re "\\|"
		       "\\(\\<external\\>\\)" )))
      (while (re-search-backward reg nil 'move)
	(cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	      ((match-end 1)                       ; begin|case|record|repeat
	       (if (and (looking-at "\\<case\\>") 
			(save-excursion
			  (forward-sexp 1)
			  (fpc-check-case)))
		   ()
		 (setq p endlist)
		 (while p 
		   (setcar p (1+ (car p)))
		   (setq p (cdr p)))
		 (if (looking-at "\\<record\\>")
		     (setq endlist (cdr endlist))))
	       )
	      ((match-end 2)                       ; end|until
	       (setq p endlist)
	       (while p 
		 (setcar p (1- (car p)))
		 (setq p (cdr p))
		 )
	       (setq endlist (cons -1 endlist)))
	      ((and (match-end 3) (fpc-defun-valid))   ; function|procedure
	       (if (or (not (member 0 endlist)) (null endlist)) 
		   (throw 'found t)
		 (setq p endlist)
		 (if (= (car p) 0)
		     (setq endlist (cdr endlist))
		   (while (not (= (car (cdr p)) 0)) (setq p (cdr p)))
		   (setcdr p (cdr (cdr p)))
		 ))
	       (if arg (if (> (current-column) fpc-indent-level) ()
			 (goto-char (point-min))
			 (throw 'found t)))
	       )
	      ((match-end 4)     ; external
	       (setq endlist (cons 0 endlist))
	       ))))
    nil)
  ;; check for class modifier
  (let ((curpos (point)))
    (forward-sexp -1)
    (if (looking-at "class\\>")
	nil
      (if curpos
	  (goto-char curpos))))
  )

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

(defun fpc-beg-of-case ()
  "Move backward to the beginning of the current case-statement."
  (interactive)
  (catch 'found
    (if (not (looking-at (concat "\\s \\|\\s)\\|" "case")))
	(forward-sexp 1))
    (let ((nest 0) (max -1) (func 0)
	  (reg (concat fpc-beg-block-re "\\|"
		       fpc-end-block-re )))
      (while (re-search-backward reg nil 'move)
	(cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	      ((match-end 1)                       ; begin|case|record|repeat
	       (if (and (looking-at "\\<case\\>") (>= nest max))
		   (throw 'found t)
		 (setq nest (1+ nest)
		       max (max max nest))))
	      ((match-end 2)                       ; end|until
	       (setq nest (1- nest)))
	      )))
    nil))

(defun fpc-next-case-label (&optional limit)
  "Move forward to the next label of the current case statement"
  (interactive)
  (let ((nest (if (looking-at "\\<case\\>") -1 0)) (found nil)
	(reg (concat fpc-beg-block-re "\\|"
		     fpc-end-block-re "\\|"
		     "\\(" fpc-caselabel-re "\\)" )))
    (while (and (not found)
		(re-search-forward reg limit 'move))
      (cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	    ((match-end 1)
	     (setq nest (1+ nest)))
	    ((match-end 2)
	     (setq nest (1- nest)))
	    ((match-end 3)
	     (if (and (<= nest 0) (/= (following-char) ?=))
		 (setq found t)))))
  found))


(defun fpc-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((nest 0) pos
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




(defun fpc-downcase-keywords ()
  "Downcase all Pascal keywords in the buffer."
  (interactive)
  (fpc-change-keywords 'downcase-word))

(defun fpc-upcase-keywords ()
  "Upcase all Pascal keywords in the buffer."
  (interactive)
  (fpc-change-keywords 'upcase-word))

(defun fpc-capitalize-keywords ()
  "Capitalize all Pascal keywords in the buffer."
  (interactive)
  (fpc-change-keywords 'capitalize-word))

;; Change the keywords according to argument.
(defun fpc-change-keywords (change-word)
  (save-excursion
    (let ((keyword-re (concat "\\<\\("
			      (mapconcat 'identity fpc-keywords "\\|")
			      "\\)\\>")))
      (goto-char (point-min))
      (while (re-search-forward keyword-re nil t)
	(funcall change-word -1)))))



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
    (if (and (looking-at "\\<end;") (fpc-in-implementation)
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "{" (fpc-get-beg-of-line) t))))
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
		    (cond ((and (looking-at fpc-beg-block-re)
				(not (looking-at "class[ \t]function"))
				(not (looking-at "class[ \t]procedure")))
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
			  (setq b (progn (beginning-of-line)
					 (fpc-beg-of-defun)
					 (if (not (looking-at "class")) nil
					   (skip-chars-forward "^ \t")
					   (skip-chars-forward " \t"))
					 (skip-chars-forward "^ \t")
					 (skip-chars-forward " \t")
					 (point))
				e (progn (skip-chars-forward "a-zA-Z0-9_.")
					 (point))))
			(insert-buffer-substring (current-buffer) b e))
		      (insert " }"))))))))))

(defun fpc-interface-is-section ()
  "Returns t if the interface keyword we ar looking at is the start of
the interface section of the module"
  (if (looking-at "interface\>")
      (save-excursion
	(forward-sexp -1)
	(forward-sexp 1)
	(not (looking-at "[ \t]*=")))
    t))




;;;
;;; Indentation
;;;
(defconst fpc-indent-alist
  '((block . (+ ind fpc-indent-level))
    (case . (+ ind fpc-case-indent))
    (caseblock . ind) (cpp . 0)
    (declaration . (+ ind fpc-indent-level))
    (paramlist . (fpc-indent-paramlist t))
    (comment . (fpc-indent-comment t))
    (defun . (fpc-get-defun-indent))
    (contexp . ind) (section . 2)
    (uncomplete-assignement . (fpc-get-assignement-indent))
    (uncomplete-property . (+ ind fpc-indent-level))
    (directives . (+ (* 2 fpc-indent-level) ind))
    (unknown . ind) (string . 0)))

(defun fpc-indent-command ()
  "Indent for special part of code."
    (let* ((indent-str (fpc-calculate-indent))
	   (type (car indent-str))
	   (ind (car (cdr indent-str))))
      (cond ((and (eq type 'paramlist)
		  (or (memq 'all fpc-auto-lineup)
		      (memq 'paramlist fpc-auto-lineup)))
	     (fpc-indent-paramlist)
	     (fpc-indent-paramlist))
	    ((and (eq type 'declaration) (save-excursion 
					   (beginning-of-line)
					   (not (looking-at "[ \t]*$")))
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
	 (ind (car (cdr indent-str))))
    (if (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
	(search-forward ":" nil t))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (or (and (eq type 'declaration) (looking-at fpc-declaration-re))
	    (eq type 'cpp)
	    (and (looking-at fpc-sections-re) (fpc-interface-is-section)))
	()
      ;; Some things only a bit 
      (if (looking-at fpc-class-prot-re)
	  (indent-to (+ (1- ind) fpc-indent-level))
	;; Other things should have no extra indent
	(if (looking-at fpc-noindent-re)
	    (cond ((or (eq type 'defun)
		       (and (looking-at "\\<else\\>") (eq type 'case)))
		   (indent-to (eval (cdr (assoc type fpc-indent-alist)))))
		  ((and (eq type 'case) (looking-at "\\<end\\>"))
		   (let ((new-ind (fpc-check-case)))
		     (if new-ind 
			 (indent-to new-ind)
		       (indent-to ind))))
		  (t
		   (indent-to ind)))
	  ;; But most lines are treated this way:
	  (indent-to (eval (cdr (assoc type fpc-indent-alist))))
	  )))))

(defun fpc-calculate-indent ()
  "Calculate the indent of the current Pascal line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((oldpos (point)) (parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	   (elsed (looking-at "[ \t]*else\\>"))
	   (ended (looking-at "[ \t]*end\\>"))
	   (tod   (looking-at "[ \t]*\\(down\\)?to\\>"))
	   (elsecount 0)
	   (directives (looking-at (concat "[ \t]*" fpc-directives-re)))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (save-excursion
			    (goto-char (scan-lists (point) -1 1))
			    (if (and (looking-at ".*[[(]$") (> (car state) 0))
				(progn
;				  (goto-char (scan-lists (point) -1 1))
				  (forward-sexp -1)
				  (setq par (+ 2 (current-column))))
			    (setq par (1+ (current-column)))))
			  (goto-char (scan-lists (point) -1 (car state))))
			 ;;((save-excursion (beginning-of-line)
			 ;;		  (eq (following-char) ?#))
			 ;; (throw 'nesting 'cpp))
			 )
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[^ \t,:(]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:[^=]")
			         ;(looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
				 ;(= (save-excursion
				 ;     (end-of-line) (backward-sexp) (point))
				 ;   (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Nest block outwards
			    (and (looking-at fpc-beg-block-re)
				 (not (looking-at "class\\((.*)\\)?;"))
				 (or (fpc-in-implementation)
				     (not (looking-at "class[ \t]+function")))
				 (or (fpc-in-implementation)
				     (not (looking-at "class[ \t]+procedure")))
				 (not (looking-at "object;")))
			    (if (= nest 0)
				(cond ((looking-at "case\\>")
				       (throw 'nesting 'case))
				      ((looking-at "record\\>")
				       (throw 'nesting 'declaration))
				      (directives
				       (throw 'nesting 'directives))
				      (t (throw 'nesting 'block)))
			      (if (and (looking-at "case\\>") 
				       (save-excursion 
					 (forward-sexp 1)
					 (fpc-check-case)))
				  () 
				  (setq nest (1- nest)))))
			   (;--Nest block inwards
			    (looking-at fpc-end-block-re)
			    ;(if (and (looking-at "end\\s ")
				;     elsed (not complete))
				;(throw 'nesting 'block))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (and (looking-at fpc-defun-re)
				 (or (> par 0) (fpc-defun-valid)))
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
			    (looking-at fpc-declaration-re)
			    (if (or (save-excursion
				  (forward-word 1)
				  (fpc-declaration-end)
				  (forward-line 1)
				  (>= oldpos (point)))
				    (looking-at fpc-defun-re))
				(throw 'nesting 'defun)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement 
			    (and (or (not complete)) (not elsed)
				 (looking-at fpc-sub-block-re))
			    (throw 'nesting 'block))
			   (;--another else
			    (and elsed (= nest 0) (looking-at "else\\>"))
			    (setq elsecount (1+ elsecount))) 
			   (;--If Statement with else
			    (and elsed (= 0 nest) 
				 (looking-at "\\<if\\>"))
			    (if (= elsecount 0) 
				(throw 'nesting 'block)
			      (setq elsecount (1- elsecount))))
			   (;--- looking at a to
			    (looking-at "\\<\\(down\\)?to\\>")
			    (setq tod t))
			   (;--Uncomplete property definition
			    (and (not complete)
				 (looking-at "property\\>")
				 (not (looking-at 
				       "property\\>.*;\([ \t]*\{.*\}\)*$")))
			    (throw 'nesting 'uncomplete-property))
			   (;--Uncomplete assignement statement
			    (and (not complete)
				 (looking-at "[^;\n]*:=")
				 (not (or elsed ended tod 
					  (looking-at ".*:=.*;"))))
			    (throw 'nesting 'uncomplete-assignement))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   (;-- a new section of code
			    (looking-at fpc-sections-re)
			    (throw 'nesting 'section))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'unknown))
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
    (if (looking-at "[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:[^=]")
	;(looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
	(if (looking-at ".*:[ \t]*$")
	    (progn (re-search-forward "[ \t]*" nil t)
		   (move-to-column (+ (current-column) 2) t))
	  (search-forward ":" nil t)
	  (skip-chars-forward " \t"))
      (if (looking-at ".*=[ \t]*\\(packed[ \t]+\\)?record\\>")
	  (search-forward "=" nil t))
      (skip-chars-forward " \t"))
    (current-column)))

(defun fpc-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward "(\\*\\|{" nil t)
		    (+ 2 (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(indent-to stcol))))

(defun fpc-indent-case ()
  "Indent within case statements."
  (skip-chars-forward ": \t")
  (let ((end (progn
	       (end-of-line)
	       (point-marker)))
	(beg (progn 
	       (fpc-beg-of-case)
	       (point))) 
	oldpos
	(ind 0))
    ;; Get right indent
    (while (< (point) (marker-position end))
      (if (not (fpc-next-case-label
		(marker-position end))) ()
	(forward-char -1)
	(delete-horizontal-space)
	(if (> (current-column) ind)
	    (setq ind (current-column))))
      )
    (goto-char beg)
    (setq oldpos (marker-position end))
    ;; Indent all case statements
    (while (< (point) (marker-position end))
      (if (fpc-next-case-label (marker-position end))
	  (forward-char -1))
      (indent-to (1+ ind))
      (if (/= (following-char) ?:)
	  ()
	(forward-char 1)
	(if (= (following-char) ?=)
	    ()
	  (delete-horizontal-space)
	  (insert " "))
	(if (looking-at ".*:")
	    (progn (re-search-forward
		    ".*:" (marker-position end) 'move)
		   )
	  ))
      (setq oldpos (point))
      )
    (goto-char oldpos)))

(defun fpc-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional arg is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point-marker)) 
	   newvar newconst
	   (stpos (progn (goto-char (1+ (scan-lists (point) -1 1))) (point)))
	   (stcol (if (not (looking-at "[ \t]*$"))
		      (current-column)
		    (save-excursion (forward-char -1)
		    (forward-sexp -1)
		    (+ 2 (current-column)))))
		    
	   (edpos (progn (fpc-declaration-end)
			 (search-backward ")" (fpc-get-beg-of-line) t)
			 (if (> (point) stpos) (point-marker)
			   (forward-line 1) (point-marker))))
	   (usevar (re-search-backward "\\<var\\>" stpos t))
	   (useconst (progn 
		       (goto-char (marker-position edpos))
		       (re-search-backward "\\<const\\>" stpos t)))
	   (inpos (progn
		    (goto-char oldpos)
		    (beginning-of-line)
		    (if (< (point) stpos) (goto-char stpos)) 
		    (if useconst
			(if (not usevar) 
			    (if (looking-at "[ \t]*const\\>")
				(prog1 stcol (setq newconst t))
			      (+ 6 stcol))
			  (if (looking-at "[ \t]*const\\>")
			      (prog1 stcol (setq newconst t))
			    (if (looking-at "[ \t]*var\\>")
				(prog1 (+ 2 stcol) (setq newvar t))
			      (+ 6 stcol))))
		      (if (and usevar (not (looking-at "[ \t]*var\\>")))
			   (+ 4 stcol) 
			(prog1 stcol 
			  (setq newconst (looking-at "[ \t]*const\\>"))
			  (setq newvar (looking-at "[ \t]*var\\>"))))
		      ))))
      (if arg inpos
	(if (not (or newconst newvar))
	    ()
	  (goto-char stpos)
	  (while (< (point) (marker-position oldpos))
	    (delete-horizontal-space)
	    (cond 
	     ((and newvar (not useconst)) 
	      (if (looking-at "[ \t]*var\\>")
		  (indent-to stcol)
		(indent-to (+ 4 stcol))))
	     ((or newconst useconst) 
	      (if (looking-at "[ \t]*var\\>")
		  (indent-to (+ 2 stcol))
		(if (looking-at "[ \t]*const\\>")
		    (indent-to stcol)
		  (indent-to (+ 6 stcol))))))
	    (forward-line 1))
	  )    
	(goto-char (marker-position oldpos))
	;(forward-char 1)
	(delete-horizontal-space)
	(indent-to inpos)
	(fpc-indent-declaration nil stpos (marker-position edpos)))

      )))

(defun fpc-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s and, or `='s."
  (let ((pos (point-marker)) status)
    (if (and (not (or arg start)) (not (fpc-declaration-beg)))
	()
      (let* ((lineup 
	     (if (and (not (looking-at "\\<\\(class\\|object\\)\\((.*)\\)?;"))
		      (or (looking-at "\\<var\\>\\|\\<record\\>\\|\\<class\\>\\|\\<object\\>") 
			  arg start)) 
		 ":" "="))
	    (lineup-both (looking-at "\\<const\\>" ))
	    (class (looking-at "\\<\\(class\\|object\\)\\>"))
;	    (stpos (if start start
;		       (forward-word 2) (backward-word 1) (point)))
	    (stpos (if start start
		     (forward-line 1)
		     (skip-chars-forward " \t")
		     (point)))
	    (edpos (set-marker (make-marker)
			       (if end end
				 (max (progn (fpc-declaration-end
					      (if class 1 nil))
					     (point))
				      pos))))
	    ind
	    ind-second )

	(goto-char stpos)
	;; Indent lines in record block
	(if arg
	    (while (<= (point) (marker-position edpos))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (save-excursion
		(setq status (parse-partial-sexp stpos (point))))
	      (if (looking-at "\\<end\\>")
		  (progn (indent-to arg)
			 (set-marker edpos (1- (point))))
		(if (or (nth 4 status) (looking-at "{\\|(\\*\\|\\\\"))
		    (fpc-indent-line)
		  (if (looking-at fpc-class-prot-re)
		      (indent-to (+ 1 arg))
		    (if (and (looking-at "read\\>\\|write\\>\\|stored\\>")
			     (eq 'uncomplete-property 
				 (car (fpc-calculate-indent))))
			(indent-to (+ arg (+ fpc-indent-level
					     fpc-indent-level)))
		      (indent-to (+ arg fpc-indent-level))
		      (if (= 0 (car status))
			  ()
			(delete-horizontal-space)
			(fpc-indent-command)))))
		  (forward-line 1))))

	;; Do lineup
	(if (not lineup-both) () 
	  (setq ind-second (fpc-get-lineup-indent stpos edpos ":" nil))
	  (goto-char stpos)
	  (while (<= (point) (marker-position edpos))
	    (if (search-forward ":" (fpc-get-end-of-line) 'move)
		(forward-char -1))
	    (delete-horizontal-space)
	    (indent-to ind-second)
	    (if (not (looking-at ":"))
		(forward-line 1) ; No more indent if there is no : or =
	      (forward-char 1)
	      (delete-horizontal-space)
	      (insert " ")
	      (forward-line 1))))

	(setq ind (fpc-get-lineup-indent stpos edpos lineup class))
	(goto-char stpos)
	(while (<= (point) (marker-position edpos))
	  (if (search-forward lineup (fpc-get-end-of-line) 'move)
	      (forward-char -1))
	  (if (and class 
		   (or (save-excursion 
			 (beginning-of-line)
			 (looking-at "[ \t]*\\<\\(constructor\\|destructor\\|function\\|procedure\\|property\\)\\>"))
		   (< 0 (car (parse-partial-sexp stpos (point))))))
	      (forward-line 1)
	    (delete-horizontal-space)
	    (indent-to ind)
	    (if (not (looking-at lineup))
		(forward-line 1) ; No more indent if there is no : or =
	      (forward-char 1)
	      (delete-horizontal-space)
	      (insert " ")
	      ;; Indent record block
	      (if (and (not (looking-at "\\<class\\((.*)\\)?;"))
		       (looking-at "\\(packed[ \t]+\\)?record\\>\\|class\\>\\|object\\>"))
		  (if (looking-at "\\(packed[ \t]+\\)?record\\>")
		      (fpc-indent-declaration (current-column))
		    (fpc-indent-declaration
		     (save-excursion
		       (beginning-of-line)
		       (skip-chars-forward " \t")
		       (current-column)))))
	      (forward-line 1))))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char (marker-position pos)))))

;  "Return the indent level that will line up several lines within the region
;from b to e nicely. The lineup string is str."
(defun fpc-get-lineup-indent (b e str class)
  (save-excursion
    (let ((ind 0)
	  (reg (concat str "\\|\\(\\<record\\>\\|\\<class\\>[^;]\\|\\<object\\>[^;]\\)\\|\\(\\<end\\>\\)"))
	  (nest 0))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	(setq nest 0)
	(if (re-search-forward reg (min e (fpc-get-end-of-line 2)) 'move)
	    (progn
	      ;; Skip record blocks
	      (cond ((match-beginning 1)
		     (setq nest (1+ nest))
		     (while (and (< (point) e) (> nest 0))
		       (re-search-forward "\\(\\<record\\>\\<class\\>\\|\\<object\\>\\)\\|\\(\\<end\\>\\)"
					  e 'move)
		       (cond ((match-beginning 1)
			      (setq nest (1+ nest)))
			     ((match-beginning 2)
			      (setq nest (1- nest)))))
		     )
		    ((match-beginning 2)
		     (goto-char e))
		    (t
		     (goto-char (match-beginning 0))
		     (skip-chars-backward " \t")
		     (if (and (> (current-column) ind)
			      (or (not class)
				  (and (save-excursion
				    (beginning-of-line)
				    (not (or (looking-at "[ \t]*\\<\\(constructor\\|destructor\\|property\\|function\\|procedure\\)\\>")
					     (looking-at "[ \t]*)[ \t]*:"))))
				  (= 0 (car (parse-partial-sexp b (point)))))))
			 (setq ind (current-column)))
		     (goto-char (match-end 0))
		     (if (not (looking-at ".*\\(\\<record\\>\\|\\<class\\>\\(([^)]*)\\)?[^;]\\|\\<object\\>[^;]\\|\\<end\\>\\)" ))
			 (end-of-line))))))) ;;end-of-line added 
      ;;search only first linup char in line
      ;; In case no lineup was found
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))


(defun fpc-get-assignement-indent ()
  (save-excursion
    (re-search-backward ":=" nil 'move)
    (beginning-of-line)
    (if (looking-at ".*:=$")
	(progn (skip-chars-forward " \t")
	       (+ (current-column) fpc-indent-level))
      (re-search-forward ":=[ \t]*" nil 'move)
      (current-column))))
      
(defun fpc-get-defun-indent ()
  (let ((ind (save-excursion
	       (fpc-beg-of-defun t)
	       (current-column)))) 
    (if (or (not (looking-at  fpc-beg-block-re))
	    (looking-at "class[ \t]+function")
	    (looking-at "class[ \t]+procedure"))
      (+ ind fpc-indent-level)
      ind
    )))
    

(defun fpc-check-case ()
  "Returns nil if the in ordinary Case statement, the indentation of of the
record block if point is in the case of a variant record definition"
  (let (indent)
    (save-excursion
      (fpc-beg-of-case)
      (setq indent (fpc-calculate-indent))
      (if (not (eq (car indent) 'declaration))
	  nil
	(car (cdr indent))))))




;;;
;;; Completion
;;;
(defvar fpc-str nil)
(defvar fpc-all nil)
(defvar fpc-pred nil)
(defvar fpc-buffer-to-use nil)
(defvar fpc-flag nil)

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

(defun fpc-func-completion (type)
  ;; Build regular expression for function/procedure names
  (if (string= fpc-str "")
      (setq fpc-str "[a-zA-Z_]"))
  (let ((fpc-str (concat (cond
			     ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
			     ((eq type 'function) "\\<\\(function\\)\\s +")
			     ((eq type 'constructor) 
			      "\\<\\(constructor\\)\\s +")
			     ((eq type 'destructor) "\\<\\(destructor\\)\\s +")
			     (t "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\s +"))
			    "\\<\\(" fpc-str "[a-zA-Z0-9_.]*\\)\\>"))
	match)
    
    (if (not (looking-at 
	      "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\>"))
	(re-search-backward 
	 "\\<\\(function\\|procedure\\|constructor\\|destructor\\)\\>" nil t))
    (forward-char 1)

    ;; Search through all reachable functions
    (while (fpc-beg-of-defun)
      (if (re-search-forward fpc-str (fpc-get-end-of-line) t)
	  (progn (setq match (buffer-substring (match-beginning 2)
					       (match-end 2)))
		 (if (or (null fpc-pred)
			 (funcall fpc-pred match))
		     (setq fpc-all (cons match fpc-all)))))
      (goto-char (match-beginning 0)))))

(defun fpc-get-completion-decl ()
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurence tp `all'
  (let ((end (save-excursion (fpc-declaration-end)
			     (point)))
	match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (fpc-get-end-of-line) t)
	  ;; Traverse current line
	  (while (and (re-search-backward 
		       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|" 
			       fpc-symbol-re)
		       (fpc-get-beg-of-line) t)
		      (not (match-end 1)))
	    (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (string-match (concat "\\<" fpc-str) match)
		(if (or (null fpc-pred)
			(funcall fpc-pred match))
		    (setq fpc-all (cons match fpc-all))))))
      (if (re-search-forward "\\<record\\>" (fpc-get-end-of-line) t)
	  (fpc-declaration-end)
	(forward-line 1)))))

(defun fpc-type-completion ()
  "Calculate all possible completions for types."
  (let ((start (point))
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
	    (fpc-get-completion-decl))))))

(defun fpc-var-completion ()
  "Calculate all possible completions for variables (or constants)."
  (let ((start (point))
	goon twice)
    ;; Search for all reachable var declarations
    (while (or (fpc-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (> start (prog1 (save-excursion (fpc-end-of-defun)
					    (point))))
	    () ; Declarations not reacable
	  (if (search-forward "(" (fpc-get-end-of-line) t)
	      ;; Check parameterlist
		(fpc-get-completion-decl))
	  (setq twice 2)
	  (while (>= (setq twice (1- twice)) 0)
	    (cond ((and (re-search-forward
			 (concat "\\<\\(var\\|const\\)\\>\\|"
				 "\\<\\(begin\\|function\\|procedure\\)\\>")
			 start t)
			(not (match-end 2)))
		   ;; Check var/const declarations
		   (fpc-get-completion-decl))
		  ((match-end 2)
		   (setq twice 0)))))))))


(defun fpc-keyword-completion (keyword-list)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (mapcar #'(lambda (s)
	     (if (string-match (concat "\\<" fpc-str) s)
		 (if (or (null fpc-pred)
			 (funcall fpc-pred s))
		     (setq fpc-all (cons s fpc-all)))))
	  keyword-list))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defun fpc-completion (fpc-str fpc-pred fpc-flag)
  (save-excursion
    (let ((fpc-all nil))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use fpc-completions
      (set-buffer fpc-buffer-to-use)

      ;; Determine what should be completed
      (let ((state (car (fpc-calculate-indent))))
	(cond (;--Within a declaration or parameterlist
	       (or (eq state 'declaration) (eq state 'paramlist)
		   (and (eq state 'defun)
			(save-excursion
			  (re-search-backward ")[ \t]*:"
					      (fpc-get-beg-of-line) t))))
	       (if (or (eq state 'paramlist) (eq state 'defun))
		   (fpc-beg-of-defun))
	       (fpc-type-completion)
	       (fpc-keyword-completion fpc-type-keywords))
	      (;--Starting a new statement
	       (and (not (eq state 'contexp))
		    (save-excursion
		      (skip-chars-backward "a-zA-Z0-9_.")
		      (backward-sexp 1)
		      (or (looking-at fpc-nosemi-re)
			  (progn
			    (forward-sexp 1)
			    (looking-at "\\s *\\(;\\|:[^=]\\)")))))
	       (save-excursion (fpc-var-completion))
	       (fpc-func-completion 'procedure)
	       (fpc-keyword-completion fpc-start-keywords))
	      (t;--Anywhere else
	       (save-excursion (fpc-var-completion))
	       (fpc-func-completion 'function)
	       (fpc-keyword-completion fpc-separator-keywords))))
      
      ;; Now we have built a list of all matches. Give response to caller
      (fpc-completion-response))))

(defun fpc-completion-response ()
  (cond ((or (equal fpc-flag 'lambda) (null fpc-flag))
	 ;; This was not called by all-completions
	 (if (null fpc-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr fpc-all))
		  (match (car fpc-all))
		  (min (length match))
		  exact tmp)
	     (if (string= match fpc-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (fpc-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) fpc-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal fpc-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(fpc-flag
	 fpc-all)))

(defvar fpc-last-word-numb 0)
(defvar fpc-last-word-shown nil)
(defvar fpc-last-completions nil)

(defun fpc-complete-word ()
  "Complete word at current point.
\(See also `fpc-toggle-completions', `fpc-type-keywords',
`fpc-start-keywords' and `fpc-separator-keywords'.)"
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (fpc-str (buffer-substring b e))
	 ;; The following variable is used in fpc-completion
	 (fpc-buffer-to-use (current-buffer))
	 (allcomp (if (and fpc-toggle-completions
			   (string= fpc-last-word-shown fpc-str))
		      fpc-last-completions
		    (all-completions fpc-str 'fpc-completion)))
	 (match (if fpc-toggle-completions
		    "" (try-completion
			fpc-str (mapcar #'(lambda (elm)
					      (cons elm 0)) allcomp)))))
    ;; Delete old string
    (delete-region b e)

    ;; Toggle-completions inserts whole labels
    (if fpc-toggle-completions
	(progn
	  ;; Update entry number in list
	  (setq fpc-last-completions allcomp
		fpc-last-word-numb
		(if (>= fpc-last-word-numb (1- (length allcomp)))
		    0
		  (1+ fpc-last-word-numb)))
	  (setq fpc-last-word-shown (elt allcomp fpc-last-word-numb))
	  ;; Display next match or same string if no match was found
	  (if (not (null allcomp))
	      (insert "" fpc-last-word-shown)
	    (insert "" fpc-str)
	    (message "(No match)")))
      ;; The other form of completion does not necessarly do that.

      ;; Insert match if found, or the original string if no match
      (if (or (null match) (equal match 't))
	  (progn (insert "" fpc-str)
		 (message "(No match)"))
	(insert "" match))
      ;; Give message about current status of completion
      (cond ((equal match 't)
	     (if (not (null (cdr allcomp)))
		 (message "(Complete but not unique)")
	       (message "(Sole completion)")))
	    ;; Display buffer if the current completion didn't help 
	    ;; on completing the label.
	    ((and (not (null (cdr allcomp))) (= (length fpc-str)
						(length match)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list allcomp))
	     ;; Wait for a keypress. Then delete *Completion*  window
	     (momentary-string-display "" (point))
	     (delete-window (get-buffer-window (get-buffer "*Completions*")))
	     )))))

(defun fpc-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (fpc-str (buffer-substring b e))
	 ;; The following variable is used in fpc-completion
	 (fpc-buffer-to-use (current-buffer))
	 (allcomp (if (and fpc-toggle-completions
			   (string= fpc-last-word-shown fpc-str))
		      fpc-last-completions
		    (all-completions fpc-str 'fpc-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a keypress. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))


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
    (let ((fpc-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use fpc-completions
      (set-buffer fpc-buffer-to-use)

      (let ((fpc-str fpc-str))
	;; Build regular expression for functions
	(if (string= fpc-str "")
	    (setq fpc-str (fpc-build-defun-re "[a-zA-Z_]"))
	  (setq fpc-str (fpc-build-defun-re fpc-str)))
	(goto-char (point-min))
      
	;; Build a list of all possible completions
	(while (re-search-forward fpc-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null fpc-pred)
		  (funcall fpc-pred match))
	      (setq fpc-all (cons match fpc-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (fpc-completion-response))))

(defun fpc-goto-defun ()
  "Move to specified Pascal function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (fpc-get-default-symbol))
	 ;; The following variable is used in fpc-comp-function
	 (fpc-buffer-to-use (current-buffer))
	 (default (if (fpc-comp-defun default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label: (default " default ") ")
				     'fpc-comp-defun nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'fpc-comp-defun nil t ""))))
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (fpc-build-defun-re label t))
	  (beginning-of-line)))))



;;;
;;; Fpc-outline-mode
;;;
(defvar fpc-outline-map nil "Keymap used in Pascal Outline mode.")

(if fpc-outline-map
    nil
  (if (boundp 'set-keymap-name)
      (set-keymap-name fpc-outline-map 'fpc-outline-map))
  (if (not (boundp 'set-keymap-parent))
      (setq fpc-outline-map (copy-keymap fpc-mode-map))
    (setq fpc-outline-map (make-sparse-keymap))
    (set-keymap-parent fpc-outline-map fpc-mode-map))
  (define-key fpc-outline-map "\M-\C-a"  'fpc-outline-prev-defun)
  (define-key fpc-outline-map "\M-\C-e"  'fpc-outline-next-defun)
  (define-key fpc-outline-map "\C-c\C-d" 'fpc-outline-goto-defun)
  (define-key fpc-outline-map "\C-c\C-s" 'fpc-show-all)
  (define-key fpc-outline-map "\C-c\C-h" 'fpc-hide-other-defuns))

(defvar fpc-outline-mode nil "Non-nil while using Pascal Outline mode.")
(make-variable-buffer-local 'fpc-outline-mode)
(set-default 'fpc-outline-mode nil)
(if (not (assoc 'fpc-outline-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(fpc-outline-mode " Outl")))))

(defun fpc-outline (&optional arg)
  "Outline-line minor mode for Pascal mode.
When in Pascal Outline mode, portions
of the text being edited may be made invisible. \\<fpc-outline-map>

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
  (interactive "P")
  (setq fpc-outline-mode
	(if (null arg) (not fpc-outline-mode) t))
  (if (boundp 'redraw-mode-line)
      (redraw-mode-line))
  (if fpc-outline-mode
      (progn
	(setq selective-display t)
	(use-local-map fpc-outline-map))
    (progn
      (setq selective-display nil)
      (fpc-show-all)
      (use-local-map fpc-mode-map))))

(defun fpc-outline-change (b e fpc-flag)
  (let ((modp (buffer-modified-p)))
    (unwind-protect
	(subst-char-in-region b e (if (= fpc-flag ?\n)
				      ?\^M ?\n) fpc-flag)
      (set-buffer-modified-p modp))))

(defun fpc-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (fpc-outline-change (point-min) (point-max) ?\n))

(defun fpc-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
			  (fpc-beg-of-defun))
		      (point)))
	  (end (progn (fpc-end-of-defun)
		      (backward-sexp 1)
		      (search-forward "\n\\|\^M" nil t)
		      (point)))
	  (opoint (point-min)))
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" beg 'move)
	(fpc-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	;; Functions may be nested
	(if (> (progn (fpc-end-of-defun) (point)) beg)
	    (goto-char opoint)))
      (if (> beg opoint)
	  (fpc-outline-change opoint (1- beg) ?\^M))

      ;; Show current function
      (fpc-outline-change beg end ?\n)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" end 'move)
	(setq opoint (point))
	(fpc-end-of-defun)
	(fpc-outline-change opoint (point) ?\^M))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" nil 'move)
	(fpc-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	(fpc-end-of-defun))
      (fpc-outline-change opoint (point-max) ?\^M)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
	  (progn
	    (goto-char beg)
	    (fpc-end-of-defun)
	    (backward-sexp 1)
	    (fpc-outline-change (point) (point-max) ?\^M))))))

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

;;; Tags for inserting pascal language constructs 
;;; code mainly copied from html-helper-mode

(defvar fpc-use-expert-menu nil
  "*If not nil, then use the full pascal menu.")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar fpc-types-to-install
  '(control types  misc)
  "*List of tag types to install when html-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

(require 'tempo)			;essential part of html-helper-mode
(condition-case nil			;menu support, standard in emacs19
    (require 'auc-menu)			;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))		;package have to have two names?

(defvar fpc-mode-menu nil
  "Menu for pascal Clobbered and rebuilt by `html-helper-install-menu'")

(defconst fpc-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `fpc-add-type-to-alist'.")

;;{{{ accessor functions for fpc-type-alist

(defun fpc-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type fpc-type-alist))))

(defun fpc-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type fpc-type-alist))))

(defun fpc-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type fpc-type-alist))))

(defun fpc-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type fpc-type-alist))))

(defun fpc-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (fpc-menu-string-for type)
	(eval (fpc-menu-for type))))

;;}}}

(defun fpc-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq fpc-type-alist (cons type fpc-type-alist)))

;; Here are the types provided by fpc-mode.
(mapcar 'fpc-add-type-to-alist
  '((control . (fpc-control-map "\C-c\C-s" fpc-control-menu "Insert Control Statements"))
    (types   . (fpc-types-map "\C-c\C-t" fpc-types-menu "Insert Type Definitions"))
    (misc    . (fpc-misc-map "\C-c\C-m"  fpc-misc-menu "Insert Other Elements"))
    ))

;; Once fpc-mode is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst fpc-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun fpc-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with fpc-add-type-to-alist."
  (setq fpc-installed-types (cons type fpc-installed-types))
  (let ((keymap (fpc-keymap-for type))
	(key (fpc-key-for type))
	(menu (fpc-menu-for type))
	(menu-string (fpc-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	     (define-key fpc-mode-map key keymap)))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'fpc-install-type fpc-types-to-install)

;; special mode keys
(mapcar
 (function (lambda (l) (define-key fpc-mode-map (car l) (nth 1 l))))
 '(([67108900] tempo-forward-mark)
   ("¤" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;;{{{ fpc-add-tag function for building basic tags

(defvar fpc-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun fpc-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "fpc-" (downcase s))))


(defun fpc-add-tag (l)
  "Add a new tag to fpc-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(fpc-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (fpc-keymap-for type))
	 (menu (fpc-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (tempo-define-template (fpc-string-to-symbol name)
					 tag completer doc
					 'fpc-tempo-tags)))

    (if (null (memq type fpc-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key fpc-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'fpc-add-cookie 'fpc-add-tag)

;;}}}

;;{{{ Pascal templates

(mapcar
 'fpc-add-tag
 '(
   ;;control statements
   ;; compound statement 
   (control "b" "begin" "block" ('> "begin" (fpc-reindent-line) 'n> 'r
				    'n "end;" (fpc-reindent-line) 'n>))
   ;; if's without elses
   (control  "i"   "ifthen" "if"      ('> "if " 'p " then" 'n> 'p))
   (control  "I"   "ifbegin" "if b e" ('> "if " 'p " then" 'n>  "begin"
					   (fpc-reindent-line) 'n> 'p 'n
					   "end;" (fpc-reindent-line)
					   'n>))
   ;; if's with elses
   (control  "j"   "ife1"  "if else" ('> "if " 'p " then" 'n> 'p 'n
					   "else" (fpc-reindent-line) 'n>
					   'p))
   (control  "J"   "ife2"  "if b e b" ('> "if " 'p " then" 'n "begin"
					   (fpc-reindent-line) 'n> 'p 'n
					   "end" (fpc-reindent-line) 'n>
					   "else" 'n "begin"
					   (fpc-reindent-line) 'n> 'p 'n
					   "end;" (fpc-reindent-line) 'n>))
   (control  "h"   "ife3"  "if b else" ('> "if " 'p " then" 'n
						 "begin" (fpc-reindent-line)
						 'n> 'p 'n
						 "end" (fpc-reindent-line) 'n
						 "else" (fpc-reindent-line)
						 'n> 'p))
   (control  "H"   "ife4"  "if else b" ('> "if " 'p " then" 'n> 'p 'n
						 "else" (fpc-reindent-line)
						 'n> "begin"
						 (fpc-reindent-line) 'n> 'p
						 'n "end;" 
						 (fpc-reindent-line) 'n>))
   ;; Loops and with
   (control "r" "repeat" "repeat" ('> "repeat" 'n> 'r 'n "until " 'p ";"
				      (fpc-reindent-line)))
   (control "w" "while"  "while" ('> "while " 'p " do" 'n> 'p))
   (control "W" "with"   "with" ('> "with " 'p " do" 'n> 'p))
   (control "f" "for"    "for"  ('> "for " 'p " := " 'p " to " 'p " do" 'n>))
   (control "F" "dfor"   "for downto"  ('> "for " 'p " := " 'p " downto " 
					   'p " do" 'n>))
   ;; try statements

   (control "t" "tryexcept" "try except" ('> "try" 'n> 'r 'n "except"
					     (fpc-reindent-line) 'n> 'p 'n
					     "end;" (fpc-reindent-line) 'n))
   (control "T" "tryfinally" "try finally" ('> "try" 'n> 'r "finally"
					     (fpc-reindent-line) 'n> 'p 'n
					     "end;" (fpc-reindent-line) 'n))

   (types     "a"   "array"   "array" ("array[" 'p "] of " 'p ";"))
   (types     "A"   "parray"   "array (packed)" ("packed array[" 'p "] of " 
						 'p ";"))
   (types     "c"   "class" "class" ("class(" 'p ")" 'n> 'p 'n "end;"
				     (fpc-reindent-line)))
   (types     "d"   "double" "double" ("double"))
   (types     "i"   "integer"   "integer" ("integer"))
   (types     "s"   "string"   "string" ("string"))
   (types     "S"   "set"   "set" ("set of"))
   (types     "r"   "record"    "record" ("record" '> 'n> 'p 'n> "end;" 
					  (fpc-reindent-line)))
   (types     "R"   "precord"    "record (packed)" ("packed record" '> 'n> 'p
						    'n> "end;" 
						    (fpc-reindent-line)))
   (types     "v"   "variant"   "variant" ("variant"))
   (misc  "c"  "Comment"          "Comment"  ("{ " 'p " }"))

   ))


;;}}}

(provide 'fpc-mode)

;;; fpc-mode.el ends here
