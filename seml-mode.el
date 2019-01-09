;;; seml-mode.el --- major-mode for SEML file        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/conao3/seml-mode
;; Package-Requires: ((emacs "22.0"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Const
;;

(defconst seml-mode-syntax-table lisp-mode-syntax-table
  "seml-mode-symtax-table")

(defconst seml-mode-keywords-regexp
  (regexp-opt '("html" "head" "body" "title" "style"
                "section" "nav" "article" "header" "footer"
                "div" "form" "input")))

(defconst seml-mode-font-lock-keywords
  `(,seml-mode-keywords-regexp))

(defconst seml-html-single-tags
  '(base link meta img br area param hr col option input wbr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  functions
;;

(defun seml-indent-function (indent-point state)
  "seml indent calc function"
  (let ((normal-indent (current-column))
        (method        1))
    (lisp-indent-specform method state indent-point normal-indent)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main
;;

(define-derived-mode seml-mode lisp-mode "SEML"
  "Major mode for editing SEML (S-Expression Markup Language) file."

  (set-syntax-table seml-mode-syntax-table)
  (setq-local font-lock-defaults '(seml-mode-font-lock-keywords nil nil))
  
  (set (make-local-variable 'lisp-indent-function) 'seml-indent-function))

(provide 'seml-mode)
;;; seml-mode.el ends here
