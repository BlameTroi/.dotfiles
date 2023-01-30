;;; txbpas.el -- Major mode for editing Pascal files in Emacs -*- lexical-binding: t -*-

;; Author: Troy Brumley <blametroi@gmail.com>
;; Created: 28 January 2023
;; Keywords: TXBPAS Pascal major-mode

;; Copyright (C) 2023 Troy Brumley <blametroi@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; After too much time trying to debug the standard pascal.el and
;; its variants, I'm taking Scott Borton's WPDL example from the
;; emacs wiki https://www.emacswiki.org/emacs/ModeTutorial and
;; building my own Pascal mode on top of it. Scott's original
;; tutorial was at: http://two-wugs.net/emacs/mode-tutorial.html
;;
;; I don't expect this to be as complete as pascal.el tried to be,
;; but it's indenting and some other behavior was so bad that I
;; decided starting over was the better approach for me. Somone
;; more familiar with emacs and lisp might reach a different
;; decision.
;;

;;; TODO:
;;
;;  1) Reasonable syntax highlighting
;;  2) ...

;;; Code:
(defvar txbpas-mode-hook nil)
(defvar txbpas-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j"     'newline-and-indent)
    (define-key map "\M-\t"    'completion-at-point)
    (define-key map "\M-?"     'completion-help-at-point)
    (define-key map "\177"     'backward-delete-char-untabify)
    map)
  "Keymap used in the TXBPAS Pascal mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pas\\'" . txbpas-mode))

;; txb: Pascal subset, no attempt at optimizing the regexps yet.
;; regexp-opt these when we finish up.
(defconst txbpas-font-lock-keywords-1
  (list
   ; These are standard keywords in Pascal.
      '("\\<\\(program\\|function\\|procedure\\|label\\|const\\|type\\|var\\|begin\\|repeat\\|case\\|of\\|for\\|while\\|with\\|do\\|if\\|then\\|else\\|end\\|until\\|to\\|downto\\)\\>" . font-lock-builtin-face)
  ;; '("\\('\\w*'\\)" . font-lock-normal-face)
   )
  "Minimal highlighting expressions for TXBPAS mode.")

(defconst txbpas-font-lock-keywords-2
  (append txbpas-font-lock-keywords-1
		  (list
           '("\\<\\(integer\\|char\\|real\\|string\\|array\\|packed\\|boolean\\|record\\|set\\)\\>" . font-lock-type-face)
           '("\\<\\(true\\|false\\|nil\\|maxint\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in TXBPAS mode.")

(defconst txbpas-font-lock-keywords-3
  (append txbpas-font-lock-keywords-2
		  (list
           '("\\<\\(and\\|or\\|not\\|div\\|mod\\|ord\\|pred\\|chr\\|sqr\\|sqrt\\|succ\\|round\\|trunc\\|sin\\|cos\\|tan\\|arctan\\|exp\\|ln\\|abs\\)\\>" . font-lock-builtin-face)
           '("\\<\\(input\\|output\\|assign\\|rewrite\\|close\\|rewind\\|get\\|put\\|read\\|readln\\|write\\|writeln\\|eof\\|eoln\\)\\>" . font-lock-function-face)))
  "And even more highlighting in TXBPAS mode.")

(defvar txbpas-font-lock-keywords txbpas-font-lock-keywords-3
  "Default highlighting expressions for TXBPAS mode.")

;; txb: very simplistic rules for pascal indent
;;
;; Ignoring proper use of semicolons, and assuming that begin and end
;; are always used even when a single statement would stand alone.
;;
;; * At beginning of buffer indent to column 0.
;; * If prior line ends with begin, increase indent.
;; * If the prior line ends with end, decrease indent.
;; * If the prior line began with if or ends with then, increase indent.
;; * If the prior line began with or ends with else, increase indent.
;; * If the prior line began with for or ends with do, increase indent.
;; * If the prior line began with while or ends with do, increase indent.
;; * If the prior line began with repeat, increase indent.
;; * If the prior line ends until, decrease indent.
;; * Case -- tbd.
;; * With -- tbd.
;; * type var const uses???
;;

(defun txbpas-should-indent ()
  "Does the prior line tell us to indent?
Lines ending with then/else/do/begin or beginning with case/repeat
trigger an indent."
  (interactive) ;; not sure this is needed
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (if (looking-at "^begin$")
        (progn
          ())
    )
  )
  )

;;
(defun txbpas-indent-line ()
  "Indent current line as TXBPAS code."
  (interactive)
  (beginning-of-line)
  (if
      (bobp)
	  (indent-line-to 0)		   ; First line is always non-indented
	(let ((not-indented t) cur-indent)
	  (if (looking-at "^[ \t]*end")
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0) ; We can't indent past the left margin
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented ; Iterate backwards until we find an indentation hint
			(forward-line -1)
			(if (looking-at "^[ \t]*end") ; This hint indicates that we need to indent at the level of the END_ token
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at "^[ \t]*\\(PARTICIPANT\\|MODEL\\|APPLICATION\\|WORKFLOW\\|ACTIVITY\\|DATA\\|TOOL_LIST\\|TRANSITION\\)") ; This hint indicates that we need to indent an extra level
				  (progn
					(setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
					(setq not-indented nil))
				(if (bobp)
					(setq not-indented nil)))))))
	  (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar txbpas-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "."   st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    ;; This used to use comment-syntax `b'.  But the only document I could
    ;; find about the syntax of Pascal's comments said that (* ... } is
    ;; a valid comment, just as { ... *) or (* ... *) or { ... }.
    (modify-syntax-entry ?* ". 23" st)
    ;; Allow //...\n comments as accepted by Free Pascal (bug#13585).
    (modify-syntax-entry ?/ ". 12c" st)
    (modify-syntax-entry ?\n "> c" st)
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
    (modify-syntax-entry ?_ "w"    st)
    (modify-syntax-entry ?\' "\""  st)
    st)
  "Syntax table for txbpas-mode")

(defun txbpas-mode ()
  "Major mode for editing Pascal source code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map txbpas-mode-map)
  (set-syntax-table txbpas-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(txbpas-font-lock-keywords))
  ;; Register our indentation function
  ;; txb: not yet we don't!
  (set (make-local-variable 'indent-line-function) 'txbpas-indent-line)
  (setq major-mode 'txbpas-mode)
  (setq mode-name "TXBPAS")
  (run-hooks 'txbpas-mode-hook))

(provide 'txbpas)

;;; txbpas-mode.el ends here
