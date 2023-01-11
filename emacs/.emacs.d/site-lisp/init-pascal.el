;;; init-pascal.el --- pascal mode initialization    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: lisp, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; TODO: how to defer?
;; i think this is where i would load a customized version in place
;; of the standard pascal-mode

;; the default pascal mode seems a bit broken, so i may
;; be tweaking it.

;; User customization for Pascal mode
(setq pascal-indent-level       2
      pascal-case-indent        2
      pascal-auto-newline       nil
      pascal-tab-always-indent  t
      pascal-auto-endcomments   nil
      pascal-auto-lineup        '(all)
      pascal-type-keywords      '("array" "file" "packed" "char"
				                  "integer" "real" "string" "record")
      pascal-start-keywords     '("begin" "end" "function" "procedure"
				                  "repeat" "until" "while" "read" "readln"
				                  "reset" "rewrite" "write" "writeln")
      pascal-separator-keywords '("downto" "else" "mod" "div" "then"))

(provide 'init-pascal)
;;; init-pascal.el ends here
