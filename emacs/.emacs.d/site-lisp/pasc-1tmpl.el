;;; pasc-1tmpl.el  Case insensitive templates for pascal mode. -*- lexical-binding: t -*-
;; Copyright (c) 1992 Inge Frick (I would not mind moving the copyright to FSF)
;; Address: i...@nada.kth.se
;;; This file contains templates used in pascal-mode when id-case-significant
;;; is off, so that case is insignificant in identifier and keyword names.

;;; The convention used is: keywords are in upper case, functions, procedures,
;;; constants and variables are capitalized and types are in lower case.
;;; Change this to suit your pascal and your own taste.

(if (null pascal-nocase-template-table)
    (setq pascal-nocase-template-table (make-abbrev-table)))

(pascal-add-templates pascal-nocase-template-table
 '(
   ;; Change this for
   ("pg" "PROGRAM ~@(Input, Output)~<;>\nBEGIN\n~<statements>\nEND." t)
     ;;; For a pascal that does want Input, Output in the program header.
   ("pr" "PROCEDURE ~@()~<;>" t)
   ("fn" "FUNCTION ~@():~<type;>" t)
   ("ex" "EXTERN;\n" t)            ; maybe change to EXTERNAL
   ("fw" "FORWARD;\n" t)
   ("lb" "LABEL" t)
   ("cn" "CONST" t)
   ("tp" "TYPE" t)
   ("vr" "VAR" t)
   ("ar" "ARRAY [~@] OF~<type>" t)
   ("fl" "FILE OF" t)
   ("st" "SET OF" t)
   ("rc" "RECORD\n~@\nEND~<;>" t)
   ("par" "PACKED ARRAY [~@] OF~<type>" t)
   ("pst" "PACKED SET OF" t)
   ("prc" "PACKED RECORD\n~@\nEND~<;>" t)
   ("be" "BEGIN ~@ END~<>" t)
   ("bg" "BEGIN\n~@\nEND~<>" t)
   ("if" "IF ~@ THEN~<statement>" t)
   ("ie" "IF ~@ THEN~<statement>\nELSE~<statement>" t)
   ("ei" "ELSE IF ~@ THEN~<statement>" t (pascal-abbrev-no-prev-semi))
   ("el" "ELSE ~@" t (pascal-abbrev-no-prev-semi))
   ("cs" "CASE ~@ OF\n~<cases>\nEND~<>" t)
   ("ot" "OTHERS:" t)                      ; maybe change to ELSE:
   ("fr" "FOR ~@:=~<start> TO~<end> DO~<statement>" t)
   ("rp" "REPEAT\n~@\nUNTIL ~<end expression>" t)
   ("wh" "WHILE ~@ DO~<statement>" t)
   ("wt" "WITH ~@ DO~<statement>" t)
;  ("lp" "LOOP\n~<statements>\nEXIT IF ~@\n~<statements>\nEND~<>")
   ("rd" "Read(~@)")
   ("rdl" "Readln")
   ("wr" "Write(~@)")
   ("wrl" "Writeln")
   ("rs" "Reset(~@)")
   ("rw" "Rewrite(~@)")
   ("gt" "Get(~@)")
   ("pt" "Put(~@)")
   ("nw" "New(~@)")
   ("ds" "Dispose(~@)")
   ("pk" "Pack(~@)")
   ("upk" "Unpack(~@)")
   ("b" "boolean")
   ("c" "char")
   ("i" "integer")
   ("r" "real")
   ("tx" "text")
   ("ip" "Input")
   ("op" "Output")
   ("mi" "MaxInt")
   ("f" "False")
   ("t" "True")
   ("nm" "" nil (pascal-proc-name))
   ))
