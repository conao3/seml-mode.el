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

(defcustom seml-indent-offset 2
  "Offset for SEML indentation."
  :type 'integer
  :group 'seml)

(defvar seml-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for SEML mode.")

(define-derived-mode seml-mode prog-mode "SEML"
  "Major mode for editing SEML (S-Expression Markup Language) file."
  :group 'seml
  (unless noninteractive
    (require 'elec-pair)
    (defvar electric-pair-text-pairs)
    (setq-local electric-pair-text-pairs
                (append '((?\` . ?\') (?‘ . ?’)) electric-pair-text-pairs))
    (setq-local electric-quote-string t)))
  
(provide 'seml-mode)
;;; seml-mode.el ends here
