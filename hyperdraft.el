;;; hyperdraft.el --- Browse documentation from ISLisp HyperDraft

;; Copyright 1997 Naggum Software
;; Copyright 2017 Minejima International

;; Author: Yuji Minejima <yuji@minejima.jp>
;; Keywords: lisp

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; This package is an adaptation of Erik Naggum's hyperspec.el for ISLisp.
;; Enter `M-x islisp-hyperdraft' and the name of ISLisp items(functions, variables, etc).
;; Then the default web browser shows that part of ISLispHyperDraft (ISO ISLisp specification
;; draft).

;;; Code:

(require 'cl)
(require 'browse-url)                   ;you need the Emacs 20 version
(require 'thingatpt)

(defvar islisp-hyperdraft-root
  "http://minejima.jp/HyperDraft/islisp-v23.html"
  "The root of ISLisp HyperDraft URL.
If you copy the Hyperdraft to your local system, set this variable to
something like \"file:/usr/local/doc/HyperDraft/islisp-v23.html\".")

(defvar islisp-hyperdraft-history nil
  "History of symbols looked up in the ISLisp HyperDraft.")

(defvar islisp-hyperdraft-symbols (make-vector 67 0))

(defun islisp-hyperdraft-strip-package (name)
  (if (string-match "^\\([^:]*\\)::?\\([^:]*\\)$" name)
      (let ((package-name (match-string 1 name))
	    (symbol-name (match-string 2 name)))
	(if (member (downcase package-name) 
		    '("cl" "common-lisp"))
	    symbol-name
	  name))
    name))

(defun islisp-hyperdraft (symbol-name)
  "View the documentation on SYMBOL-NAME from The ISLisp HyperDraft.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions."
  (interactive (list (let* ((symbol-at-point (thing-at-point 'symbol))
			    (stripped-symbol 
			     (and symbol-at-point
				  (downcase
				   (islisp-hyperdraft-strip-package 
				    symbol-at-point)))))
                       (if (and stripped-symbol
                                (intern-soft stripped-symbol
                                             islisp-hyperdraft-symbols))
                           stripped-symbol
                         (completing-read
                          "Look up symbol in ISLisp HyperDraft: "
                          islisp-hyperdraft-symbols #'boundp
                          t stripped-symbol
                          'islisp-hyperdraft-history)))))
  (maplist (lambda (entry)
             (browse-url (concat islisp-hyperdraft-root "#" (car entry)))
             (if (cdr entry)
                 (sleep-for 1.5)))
           (let ((symbol (intern-soft 
			  (islisp-hyperdraft-strip-package 
			   (downcase symbol-name))
			  islisp-hyperdraft-symbols)))
             (if (and symbol (boundp symbol))
                 (symbol-value symbol)
               (error "The symbol `%s' is not defined in ISLisp"
                      symbol-name)))))

(mapcar (lambda (entry)
          (let ((symbol (intern (car entry) islisp-hyperdraft-symbols)))
            (if (boundp symbol)
                (push (cadr entry) (symbol-value symbol))
              (set symbol (cdr entry)))))
        '(("consp" "f_consp")
          ("car" "f_car")
          ("cdr" "f_cdr")
          ("cons" "f_cons")
          ("set-car" "f_set_car")
          ("set-cdr" "f_set_cdr")
          ("null" "f_null")
          ("listp" "f_listp")
          ("create-list" "f_create_list")
          ("list" "f_list")
          ("reverse" "f_reverse")
          ("nreverse" "f_nreverse")
          ("append" "f_append")
          ("member" "f_member")
          ("mapcar" "f_mapcar")
          ("mapc" "f_mapc")
          ("mapcan" "f_mapcan")
          ("maplist" "f_maplist")
          ("mapl" "f_mapl")
          ("mapcon" "f_mapcon")
          ("assoc" "f_assoc")
          ("basic-array-p" "f_basic_array_p")
          ("basic-array*-p" "f_basic_array_s_p")
          ("general-array*-p" "f_general_array_s_p")
          ("create-array" "f_create_array")
          ("aref" "f_aref")
          ("garef" "f_garef")
          ("set-aref" "f_set_aref")
          ("set-garef" "f_set_garef")
          ("array-dimensions" "f_array_dimensions")
          ("basic-vector-p" "f_basic_vector_p")
          ("general-vector-p" "f_general_vector_p")
          ("create-vector" "f_create_vector")
          ("vector" "f_vector")
          ("stringp" "f_stringp")
          ("create-string" "f_create_string")
          ("string=" "f_stringeq")
          ("string/=" "f_stringneq")
          ("string<" "f_stringlt")
          ("string>" "f_stringgt")
          ("string>=" "f_stringgteq")
          ("string<=" "f_stringlteq")
          ("char-index" "f_char_index")
          ("string-index" "f_string_index")
          ("string-append" "f_string_append")
          ("length" "f_length")
          ("elt" "f_elt")
          ("set-elt" "f_set_elt")
          ("subseq" "f_subseq")
          ("map-into" "f_map_into")
          ("streamp" "f_streamp")
          ("open-stream-p" "f_open_stream_p")
          ("input-stream-p" "f_input_stream_p")
          ("output-stream-p" "f_output_stream_p")
          ("standard-input" "f_standard_input")
          ("standard-output" "f_standard_output")
          ("error-output" "f_error_output")
          ("open-input-file" "f_open_input_file")
          ("open-output-file" "f_open_output_file")
          ("open-io-file" "f_open_io_file")
          ("close" "f_close")
          ("finish-output" "f_finish_output")
          ("create-string-input-stream" "f_create_string_input_stream")
          ("create-string-output-stream" "f_create_string_output_stream")
          ("get-output-stream-string" "f_get_output_stream_string")
          ("read" "f_read")
          ("read-char" "f_read_char")
          ("preview-char" "f_preview_char")
          ("read-line" "f_read_line")
          ("stream-ready-p" "f_stream_ready_p")
          ("format" "f_format")
          ("format-char" "f_format_char")
          ("format-float" "f_format_float")
          ("format-fresh-line" "f_format_fresh_line")
          ("format-integer" "f_format_integer")
          ("format-object" "f_format_object")
          ("format-tab" "f_format_tab")
          ("read-byte" "f_read_byte")
          ("write-byte" "f_write_byte")
          ("probe-file" "f_probe_file")
          ("file-position" "f_file_position")
          ("set-file-position" "f_set_file_position")
          ("file-length" "f_file_length")
          ("error" "f_error")
          ("cerror" "f_cerror")
          ("signal-condition" "f_signal_condition")
          ("condition-continuable" "f_condition_continuable")
          ("continue-condition" "f_continue_condition")
          ("arithmetic-error-operation" "f_arithmetic_error_operation")
          ("arithmetic-error-operands" "f_arithmetic_error_operands")
          ("domain-error-object" "f_domain_error_object")
          ("domain-error-expected-class" "f_domain_error_expected_class")
          ("parse-error-string" "f_parse_error_string")
          ("parse-error-expected-class" "f_parse_error_expected_class")
          ("simple-error-format-string" "f_simple_error_format_string")
          ("simple-error-format-arguments" "f_simple_error_format_arguments")
          ("stream-error-stream" "f_stream_error_stream")
          ("undefined-entity-name" "f_undefined_entity_name")
          ("undefined-entity-namespace" "f_undefined_entity_namespace")
          ("identity" "f_identity")
          ("get-universal-time" "f_get_universal_time")
          ("get-internal-run-time" "f_get_internal_run_time")
          ("get-internal-real-time" "f_get_internal_real_time")
          ("internal-time-units-per-second" "f_internal_time_units_per_second")
          
          ))
                
(provide 'hyperdraft)

;;; hyperdraft.el ends here
