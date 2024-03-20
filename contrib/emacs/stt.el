;;; stt.el --- Major mode for stt -*- lexical-binding: t; coding: utf-8 -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'comint)
(require 'font-lock)

(defgroup stt nil
  "Editing and running stt files."
  :group 'languages)

(defgroup stt-faces nil
  "Faces for stt-mode."
  :group 'font-lock)

(defface stt-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for stt keywords."
  :group 'stt-faces)

(defface stt-type-face
  '((t (:inherit font-lock-type-face)))
  "Face for stt type names."
  :group 'stt-faces)

(defface stt-builtin-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for stt builtin functions."
  :group 'stt-faces)

(defface stt-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Face for stt constant values."
  :group 'stt-faces)

(defcustom stt-keywords
  '("case" "def" "fix" "in" "is" "let" "of" "sig" "λ")
  "A list of stt keywords to highlight with `stt-keyword-face'."
  :group 'stt
  :type '(repeat string))

(defcustom stt-builtin-types
  '("Any" "Bool" "Empty" "Int")
  "A list of stt types to highlight with `stt-type-face'."
  :group 'stt
  :type '(repeat string))

(defcustom stt-builtin-functions
  '("fst" "snd")
  "A list of stt values to highlight with `stt-builtin-face'."
  :group 'stt
  :type '(repeat string))

(defcustom stt-builtin-constants
  '("true" "false")
  "A list of stt values to highlight with `stt-constant-face'."
  :group 'stt
  :type '(repeat string))

(defvar stt-mode-syntax-table nil
  "Syntax table for stt-mode.")

(setq stt-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}"    table)
    (modify-syntax-entry ?\} "){"    table)
    (modify-syntax-entry ?\( "()"    table)
    (modify-syntax-entry ?\) ")("    table)
    (modify-syntax-entry ?\" "\""    table)
    (modify-syntax-entry ?\' "w"     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?~  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  ". 12b" table)
    (modify-syntax-entry ?\n "> b"   table)
    table))

(defconst stt-font-lock-defaults
  `((,(regexp-opt stt-keywords 'words)          . 'stt-keyword-face)
    (,(regexp-opt stt-builtin-functions 'words) . 'stt-builtin-face)
    (,(regexp-opt stt-builtin-constants 'words) . 'stt-constant-face)
    (,(regexp-opt stt-builtin-types 'words)     . 'stt-type-face))
  "Font lock highlights for stt.")

;;;###autoload
(define-derived-mode stt-mode prog-mode "stt"
  "Major mode for stt.

\\{stt-mode-map}"
  :group 'stt
  (setq-local font-lock-defaults '(stt-font-lock-defaults))
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--+[\t ]*")
  (setq-local commend-end "")
  (setq-local indent-tabs-mode nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.stt" . stt-mode))

(provide 'stt)
;;; stt.el ends here
