;;; seml-mode-tests.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp
;; Version: 1.4.1
;; URL: https://github.com/conao3/seml-mode.el
;; Package-Requires: ((emacs "24.3") (simple-httpd "1.5") (htmlize "1.5"))

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

(load "cort-test")
(require 'seml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

(defalias 'message 'ignore)

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

(defvar seml-sample-str1 "
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>sample page</title>
    <link rel=\"stylesheet\" href=\"sample1.css\">
  </head>
  <body>
    <h1>sample</h1>
    <p>text sample</p>
  </body>
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

(defvar seml-sample-dir
  (expand-file-name "sample/"
                    (file-name-directory
                     (or load-file-name (buffer-file-name)))))

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
     (seml-encode-html-from-region (point-min) (point-max)))
   seml-sample-sexp1))

(cort-deftest seml-test:/simple-encode
  (seml-expansion
   (seml-encode-html-from-string seml-sample-str1)
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

(cort-deftest seml-test:/simple-encode-file
  (seml-expansion
   (seml-encode-html-from-file
    (expand-file-name "test-1.html" seml-sample-dir))
   seml-sample-sexp1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  decode seml
;;

(cort-deftest seml-test:/simple-decode-region
  (seml-str-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert (prin1-to-string `',seml-sample-sexp1))
       (seml-decode-seml-from-region (point-min) (point-max) "<!DOCTYPE html>")))
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode-sexp
  (seml-str-expansion
   (seml-decode-seml-from-sexp seml-sample-sexp1 "<!DOCTYPE html>")
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode-string
  (seml-str-expansion
   (seml-decode-seml-from-string
    (prin1-to-string `',seml-sample-sexp1) "<!DOCTYPE html>")
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode-current-buffer
  (seml-str-expansion
   (let ((buf (get-buffer-create (format "*seml-%s*" (gensym)))))
     (with-current-buffer buf
       (insert (prin1-to-string `',seml-sample-sexp1))
       (seml-decode-seml-from-buffer nil "<!DOCTYPE html>")))
     seml-sample-str1-decode))

(cort-deftest seml-test:/simple-decode-file
  (seml-str-expansion
   (seml-decode-seml-from-file
    (expand-file-name "test-1.seml" seml-sample-dir) "<!DOCTYPE html>")
   seml-sample-str1-decode))

(cort-deftest seml-test:/simple-comment
  (seml-str-expansion
   (seml-decode-seml-from-sexp '(comment nil " Created by htmlize-1.55 in css mode. "))
   "
<!-- Created by htmlize-1.55 in css mode. -->
"))

(cort-deftest seml-test:/simple-top
  (seml-str-expansion
   (seml-decode-seml-from-sexp '(top nil
                                     (comment nil " Created by htmlize-1.55 in css mode. ")
                                     (comment nil " Created by htmlize-1.55 in css mode. ")))
   "
<!-- Created by htmlize-1.55 in css mode. -->

<!-- Created by htmlize-1.55 in css mode. -->
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  other functions
;;

(cort-deftest seml-test:/simple-format
  (seml-str-expansion
   (seml-to-string
    '(html ((lang . "en"))
           (head nil
                 (meta ((charset . "utf-8")))
                 (title nil
                        "sample page")
                 (link ((rel . "stylesheet") (href . "sample1.css"))))
           (body nil
                 (h1 nil
                     "sample")
                 (p nil
                    "sample"
                    "text sample"))))
   "(html ((lang . \"en\"))
  (head nil
    (meta ((charset . \"utf-8\")))
    (title nil
      \"sample page\")
    (link ((rel . \"stylesheet\") (href . \"sample1.css\"))))
  (body nil
    (h1 nil
      \"sample\")
    (p nil
      \"sample\"
      \"text sample\")))
"))

(cort-deftest seml-test:/simple-pre
  (seml-str-expansion
   (seml-decode-seml-from-sexp
    '(pre nil
          "\n("
          (span ((class . "keyword")) "leaf")
          " real-auto-save\n  "
          (span ((class . "builtin")) ":ensure")
          " t\n  "
          (span ((class . "builtin")) ":custom")
          " ((real-auto-save-interval . 0.3))\n  "
          (span ((class . "builtin")) ":commands")
          " real-auto-save-mode\n  "
          (span ((class . "builtin")) ":hook")
          " (find-file-hook . real-auto-save-mode))\n"))
   "<pre>
(<span class=\"keyword\">leaf</span> real-auto-save
  <span class=\"builtin\">:ensure</span> t
  <span class=\"builtin\">:custom</span> ((real-auto-save-interval . 0.3))
  <span class=\"builtin\">:commands</span> real-auto-save-mode
  <span class=\"builtin\">:hook</span> (find-file-hook . real-auto-save-mode))
</pre>"))

(cort-deftest seml-test:/simple-xpath
  (:equal
   (seml-xpath '(html head link)
               '(html ((lang . "en"))
                      (head nil
                            (meta ((charset . "utf-8")))
                            (title nil
                                   "sample page")
                            (link ((rel . "stylesheet") (href . "sample1.css")))
                            (link ((rel . "stylesheet") (href . "sample2.css"))))
                      (body nil
                            (h1 nil
                                "sample")
                            (p nil
                               "sample"
                               "text sample"))))
   '((link
      ((rel . "stylesheet")
       (href . "sample1.css")))
     (link
      ((rel . "stylesheet")
       (href . "sample2.css"))))))

(cort-deftest seml-test:/simple-xpath-without-top
  (:equal
   (seml-xpath '(html body h2)
     '(html ((lang . "en"))
            (head nil
                  (meta ((charset . "utf-8")))
                  (title nil
                         "sample page")
                  (link ((rel . "stylesheet") (href . "sample1.css")))
                  (link ((rel . "stylesheet") (href . "sample2.css"))))
            (body nil
                  (h2 nil "sample-1")
                  (h2 nil "sample-2")
                  (h2 nil "sample-3")
                  (p nil
                     "sample"
                     "text sample")))
     t)
   '(("sample-1")
     ("sample-2")
     ("sample-3"))))

(cort-deftest seml-test:/simple-xpath-without-top
  (:equal
   (seml-xpath-without-top '(html body h2)
     '(html ((lang . "en"))
            (head nil
                  (meta ((charset . "utf-8")))
                  (title nil
                         "sample page")
                  (link ((rel . "stylesheet") (href . "sample1.css")))
                  (link ((rel . "stylesheet") (href . "sample2.css"))))
            (body nil
                  (h2 nil "sample-1")
                  (h2 nil "sample-2")
                  (h2 nil "sample-3")
                  (p nil
                     "sample"
                     "text sample"))))
   '(("sample-1")
     ("sample-2")
     ("sample-3"))))

(cort-deftest seml-test:/simple-xpath-single
  (:equal
   (seml-xpath-single '(html body)
     '(html ((lang . "en"))
            (head nil
                  (meta ((charset . "utf-8")))
                  (title nil
                         "sample page")
                  (link ((rel . "stylesheet") (href . "sample1.css")))
                  (link ((rel . "stylesheet") (href . "sample2.css"))))
            (body nil
                  (h2 nil "sample-1")
                  (h2 nil "sample-2")
                  (h2 nil "sample-3")
                  (p nil
                     "sample"
                     "text sample"))))
   '(body nil
          (h2 nil "sample-1")
          (h2 nil "sample-2")
          (h2 nil "sample-3")
          (p nil
             "sample"
             "text sample"))))

(cort-deftest seml-mode:/simple-htmlize
  (:equal (seml-htmlize 'emacs-lisp-mode "(require 'seml)")
          '(pre nil "
("
                (span ((class . "keyword")) "require")" '"
                (span ((class . "constant")) "seml") ")")))

(cort-deftest seml-mode:/simple-with-elisp
  (:equal (with-seml-elisp
            emacs-version)
          nil))

(cort-deftest seml-mode:/simple-pre-do-not-reduce-space
  (:string= (seml-decode-seml-from-sexp
             (seml-htmlize
              'emacs-lisp-mode
              "(string :tag \"Sun\")"))
            "<pre>
(string <span class=\"builtin\">:tag</span> <span class=\"string\">\"Sun\"</span>)</pre>"))

(cort-deftest seml-mode:/simple-expand
  (:string= (expand-file-name "mail" "/leaf-browser/group/applications")
            "/leaf-browser/group/applications/mail"))
(cort-deftest seml-mode:/simple-expand
  (:string= (expand-file-name "mail" "/leaf-browser/group/applications/")
            "/leaf-browser/group/applications/mail"))
(cort-deftest seml-mode:/simple-expand
  (:string= (expand-file-name "../mail" "/leaf-browser/group/applications/")
            "/leaf-browser/group/mail"))
(cort-deftest seml-mode:/simple-expand
  (:string= (expand-file-name "../mail" "/leaf-browser/group/applications//")
            "/leaf-browser/group/mail"))

(cort-deftest seml-mode:/simple-format
  (:string= (seml-to-string
             (seml-htmlize
              'emacs-lisp-mode
              (prin1-to-string
               '(debug.seml footer.seml main.seml))
              nil
              (lambda ()
                (save-excursion
                  (goto-char (point-min))
                  (forward-char)
                  (ignore-errors
                    (while t
                      (forward-sexp)
                      (insert "\n")))))))
            "(pre nil
  \"
(debug\\\\.seml
 footer\\\\.seml
 main\\\\.seml
 )\")
"))

(cort-deftest seml-mode:/simple-jade
  (:string= (seml-decode-seml-from-sexp '(h1 ("#header.class1.class2") "sample"))
            "
<h1 id=\"header\" class=\"class1 class2\">sample</h1>
"))

(cort-deftest seml-mode:/simple-jade2
  (:string= (seml-decode-seml-from-sexp '(h1 ("#header.class1") "sample"))
            "
<h1 id=\"header\" class=\"class1\">sample</h1>
"))

(cort-deftest seml-mode:/simple-jade3
  (:string= (seml-decode-seml-from-sexp '(h1 ("class1") "sample"))
            "
<h1 class=\"class1\">sample</h1>
"))

(cort-deftest seml-mode:/simple-ul
  (:string= (seml-decode-seml-from-sexp
             `(ul nil
                  ,@(mapcar (lambda (x)
                             `(li nil ,(format "item-%s" x)))
                           (number-sequence 1 5))))
            "
<ul>
<li>item-1</li>

<li>item-2</li>

<li>item-3</li>

<li>item-4</li>

<li>item-5</li>
</ul>
"))

(cort-deftest seml-mode:simple-ignore-nil
  (:string= (seml-decode-seml-from-sexp '(h1 ("#d" nil (class . "class")) "s"))
           "
<h1 id=\"d\" class=\"class\">s</h1>
"))

(cort-deftest seml-mode:simple-ignore-beginning-dot
  (:string= (seml-decode-seml-from-sexp '(h1 (".class") "s"))
           "
<h1 class=\"class\">s</h1>
"))
