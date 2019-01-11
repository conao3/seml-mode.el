;;; seml-mode.el --- major-mode for SEML file        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/conao3/seml-mode
;; Package-Requires: ((emacs "24.5"))

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

(require 'simple-httpd)

(defgroup seml nil
  "Major mode for editing SEML (S-Expression Markup Language) file."
  :group 'lisp
  :prefix "seml-")

(defcustom seml-mode-hook nil
  "Hook run when entering seml mode."
  :type 'hook
  :group 'seml)

(defvar seml-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for SEML mode.")

(defcustom seml-live-refresh-interval 1.1
  "Live-refresh interval.
NOTE: If you have auto-save settings, set this variable loger than it.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Const
;;

(defconst seml-mode-syntax-table lisp-mode-syntax-table
  "seml-mode-symtax-table")

(defconst seml-mode-keywords
  '(html
    head title base link meta style
    script noscript
    body section nav article aside hgroup header footer address
    h1 h2 h3 h4 h5 h6
    p hr pre backquote ol ul li
    dl dt dd figure figcaption div main
    a em strong small s cite q dfn addr time code var
    samp kbd sub sup i b mark ruby rt rpbdo span br wbr
    ins del
    img iframe embed object param
    video audio source canvas map area
    table caption colgroup col tbody thead tfoot tr td th
    form fieldset legend label input button select
    datalist optgroup option textarea keygen output progress meter
    details summary command menu))

(defconst seml-mode-keywords-regexp
  (eval `(rx (or ,@(mapcar 'symbol-name seml-mode-keywords)))))

(defconst seml-html-single-tags
  '(base link meta img br area param hr col option input wbr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  functions
;;

(defun seml-indent-function (indent-point state)
  "seml indent calc function"
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
			 (get (intern-soft function) 'lisp-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
              ((memq (intern-soft function) seml-mode-keywords)
               (lisp-indent-specform 1 state
				     indent-point normal-indent))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Encode
;;

;; TODO: comment feature

(defalias 'seml-encode-html-region 'libxml-parse-html-region)

(defun seml-encode-html (str)
  "encode HTML to SEML from STR."
  (with-temp-buffer
    (insert str)
    (seml-encode-html-region (point-min) (point-max))))

(defun seml-encode-html-from-buffer (&optional buf)
  (seml-encode-html-from-buffer
   (if buf (with-current-buffer buf (buffer-string))
     (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Decode
;;

(defun seml-decode-html (dom &optional doctype)
  "decode SEML[str|sexp] to html"
  (let ((dom* (if (stringp dom) (read dom) dom)))
    (concat
     (if doctype doctype "")
     (let* ((prop--fn) (decode-fn))
       (setq prop--fn
             (lambda (x)
               (format " %s=\"%s\"" (car x) (cdr x))))
       (setq decode-fn
             (lambda (dom)
               (if (listp dom)
                   (let* ((tag  (pop dom))
                          (prop (pop dom))
                          (rest dom)
                          (tagname (symbol-name tag)))
                     (if (memq tag seml-html-single-tags)
                         (format "%s\n"
                                 (format "<%s%s>" tagname (mapconcat prop--fn prop "")))
                       (format "\n%s%s%s\n"
                               (format "<%s%s>" tagname (mapconcat prop--fn prop ""))
                               (mapconcat decode-fn rest "")
                               (format "</%s>" tagname))))
                 dom)))
       (funcall decode-fn dom*)))))

(defun seml-decode-html-from-buffer (&optional buf)
  "decode from buffer."
  (seml-decode-html
   (if buf (with-current-buffer buf (buffer-string))
     (buffer-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Replace
;;

(defun seml-replace-buffer-from-html ()
  "buffer contents"
  (interactive)
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (erase-buffer)
    (insert (pp-to-string (seml-encode-html str)))
    (seml-mode)
    (indent-region (point-min) (point-max))))

(defun seml-replace-buffer-from-seml ()
  "buffer contents"
  (interactive)
  (let ((str (buffer-string)))
    (erase-buffer)
    (insert
     (seml-decode-html (read str) "<!DOCTYPE html>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Live refresh (Google chrome on macOS only)
;;

(defvar seml-live-refresh-timer nil)
(defvar seml-live-refresh-baffer "")
(defvar seml-live-refresh-prev-sexp-history nil)

(defun seml-live-refresh-start ()
  "live refresh from buffer-string (without saving)"
  (interactive)

  (if seml-live-refresh-timer
      (message "Already live refresh enabled (Taget buffer: %s)"
               seml-live-refresh-baffer)
    (setq seml-live-refresh-baffer (buffer-name))
    (setq seml-live-refresh-timer
          (run-with-idle-timer
           seml-live-refresh-interval t 'seml-live-refresh-func))
    (message "Live refresh enabled (Taget buffer: %s)"
             seml-live-refresh-baffer))

  (defservlet* seml-mode/debug/:_arg1/:_arg2/:_arg3 "text/html" (_query)
    (insert (seml-decode-html
             (with-current-buffer seml-live-refresh-baffer
               (eval
                (read
                 (buffer-substring-no-properties (point-min) (point-max)))))
             "<!DOCTYPE html>"))))

(defun seml-live-refresh-stop ()
  "live refresh from buffer-string (without saving)"
  (interactive)
  (when seml-live-refresh-timer
    (setq seml-live-refresh-baffer "")
    (cancel-timer seml-live-refresh-timer)
    (setq seml-live-refresh-timer nil)))

(defun seml-live-refresh-func ()
  "live refresh from buffer-string (without saving)"
  (let ((fn (lambda (x)
              (save-excursion
                (with-current-buffer (get-buffer-create "*seml-live-refresh*")
                  (goto-char (point-max))
                  (when (< 10 (line-number-at-pos))
                    (erase-buffer))
                  (insert x)))))
        (url) (sexp))
    (condition-case err
        (progn
          (setq sexp (eval
                      (read
                       (with-current-buffer seml-live-refresh-baffer
                         (buffer-substring-no-properties (point-min) (point-max))))))
          (setq url (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string
                      (mapconcat 'identity
                                 '("osascript -e"
                                   "'tell application \"/Applications/Google Chrome.app\""
                                   "to URL of active tab of window 1'") " "))))

          (cond ((equal sexp seml-live-refresh-prev-sexp-history)
                 (funcall fn (format "%s, Nothing to change, Abort\n"
                                     seml-live-refresh-baffer)))
                ((string-match "localhost.*seml-mode/debug" url)
                 (setq seml-live-refresh-prev-sexp-history
                       (eval (read
                              (with-current-buffer seml-live-refresh-baffer
                                (buffer-substring-no-properties (point-min) (point-max))))))

                 (shell-command-to-string
                  (mapconcat 'identity
                             '("osascript -e"
                               "'tell application \"/Applications/Google Chrome.app\""
                               "to reload active tab of window 1'") " "))

                 (setq seml-live-refresh-prev-sexp-history sexp)
                 (funcall fn (format "%s, Success.\n"
                                     seml-live-refresh-baffer)))
                (t (funcall fn (format "%s, URL is %s, Abort.\n"
                                       seml-live-refresh-baffer url)))))
      
      (error (funcall fn (format "%s, Cannot eval, Abort. (Err msg: %s)\n"
                                 seml-live-refresh-baffer err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main
;;

(define-derived-mode seml-mode emacs-lisp-mode "SEML"
  "Major mode for editing SEML (S-Expression Markup Language) file."

  (set-syntax-table seml-mode-syntax-table)
  
  (set (make-local-variable 'lisp-indent-function) 'seml-indent-function))

(add-to-list 'auto-mode-alist '("\\.seml\\'" . seml-mode))
(add-to-list 'interpreter-mode-alist '("seml" . seml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  font-lock
;;

;; (defvar seml-font-lock-keywords
;;   `(,(eval `(rx "(" (group (regexp ,seml-mode-keywords-regexp)) (* not-wordchar)
;;                 (or
;;                  (group "nil")
;;                  ;; (group "(" (+
;;                  ;;             (group (* not-wordchar)
;;                  ;;                    "(" (group (+? any)) "." (+? any) ")"
;;                  ;;                    (* not-wordchar)))
;;                  ;;        ")")
;;                  )))
;;     (1 font-lock-keyword-face)
;;     (2 font-lock-constant-face)
;;     ;; (5 font-lock-constant-face t)
;;     ))
;;
;; (font-lock-add-keywords 'seml-mode seml-font-lock-keywords)
(font-lock-add-keywords 'seml-mode
                        `((,(eval
                            `(rx "(" (group
                                      (regexp ,seml-mode-keywords-regexp)
                                      not-wordchar)))
                           (1 font-lock-keyword-face nil nil))))

(provide 'seml-mode)
;;; seml-mode.el ends here
