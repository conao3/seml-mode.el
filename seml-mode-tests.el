;;; seml-mode-tests.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/conao3/seml-mode.el
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seml-mode)
(require 'cort-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support macros
;;

(defmacro seml-expansion (given expect)
  `(:equal ,given ,expect))

(defmacro seml-str-expansion (given expect)
  `(:string= ,given ,(eval expect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support variables
;;

(defvar seml-sample-str1
  "<!DOCTYPE html>
<html lang=\"en\">
<head><meta charset=\"utf-8\"/>
<title>sample page</title>
<link rel=\"stylesheet\" href=\"sample1.css\"/>
</head>
<body><h1>sample</h1><p>text sample</p></body>
</html>")

(defvar seml-sample-str1-decode
  "<!DOCTYPE html>
<html lang=\"en\">
<head><meta charset=\"utf-8\">

<title>sample page</title>
<link rel=\"stylesheet\" href=\"sample1.css\">
</head>

<body>
<h1>sample</h1>

<p>text sample</p>
</body>
</html>
")

(defvar seml-sample-sexp1
  '(html ((lang . "en"))
         (head nil
               (meta ((charset . "utf-8")))
               (title nil "sample page")
               (link ((rel . "stylesheet") (href . "sample1.css"))))
         (body nil
               (h1 nil "sample")
               (p nil "text sample"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  encode html
;;

(cort-deftest seml-test:/simple-encode-region
  (seml-expansion
   (with-temp-buffer
     (insert seml-sample-str1)
     (seml-encode-html-region (point-min) (point-max)))
   seml-sample-sexp1))

(cort-deftest seml-test:/simple-encode
  (seml-expansion
   (seml-encode-html seml-sample-str1)
   seml-sample-sexp1))

(cort-deftest seml-test:/simple-encode-buffer
  (seml-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert seml-sample-str1))
     (seml-encode-html-from-buffer buf))
   seml-sample-sexp1))

(cort-deftest seml-test:/simple-encode-current-buffer
  (seml-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert seml-sample-str1)
       (seml-encode-html-from-buffer)))
   seml-sample-sexp1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  decode seml
;;

(cort-deftest seml-test:/simple-decode-region
  (seml-str-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert (prin1-to-string seml-sample-sexp1))
       (seml-decode-seml-region (point-min) (point-max) "<!DOCTYPE html>")))
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode
  (seml-str-expansion
   (seml-decode-seml seml-sample-sexp1 "<!DOCTYPE html>")
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode-current-buffer
  (seml-str-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert (prin1-to-string seml-sample-sexp1))
       (seml-decode-seml-from-buffer nil "<!DOCTYPE html>")))
     seml-sample-str1-decode))

(provide 'seml-mode-tests)
;;; seml-mode-tests.el ends here
