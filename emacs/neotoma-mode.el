;; neotoma-mode.el --- Major mode for editing Neotoma PEG grammars (Erlang)

;; Copyright (c) 2014 Sean Cribbs

;; (The MIT License)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(eval-when-compile (require 'cl))
(require 'erlang)

;; Customize

;;;###autoload
(defgroup neotoma nil
  "Support for Neotoma PEG grammars"
  :group 'languages
  :prefix "neotoma-")

;;;###autoload
(defcustom neotoma-mode-hook nil
  "Hook run when entering Neotoma mode"
  :type 'hook
  :group 'neotoma)

;; Define the major mode

;;;###autoload
(define-derived-mode neotoma-mode prog-mode "Neotoma"
  "Major mode for editing Neotoma PEG grammars (Erlang).

\\{neotoma-mode-map}"
  :group 'neotoma
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'comment-start) "%% ")
  (setq indent-tabs-mode nil)
  (neotoma-add-font-lock))

;; Syntax table
(modify-syntax-entry ?< "." neotoma-mode-syntax-table) ;; Left arrow in rule
(modify-syntax-entry ?- "." neotoma-mode-syntax-table) ;; Left arrow in rule
(modify-syntax-entry ?+ "." neotoma-mode-syntax-table) ;; Repetition
(modify-syntax-entry ?* "." neotoma-mode-syntax-table) ;; Repetition
(modify-syntax-entry ?? "." neotoma-mode-syntax-table) ;; Lookahead
(modify-syntax-entry ?! "." neotoma-mode-syntax-table) ;; Lookahead
(modify-syntax-entry ?/ "." neotoma-mode-syntax-table) ;; Alternation
(modify-syntax-entry ?\; "." neotoma-mode-syntax-table) ;; Rule terminator
(modify-syntax-entry ?: "." neotoma-mode-syntax-table) ;; Label delimiter
(modify-syntax-entry ?` "$" neotoma-mode-syntax-table) ;; Backticked code blocks
(modify-syntax-entry ?~ "." neotoma-mode-syntax-table) ;; Identity transformation
(modify-syntax-entry ?' "\"" neotoma-mode-syntax-table) ;; Both single and double-quotes are strings
(modify-syntax-entry ?% "<" neotoma-mode-syntax-table) ;; Erlang-style % comments
(modify-syntax-entry ?\n ">" neotoma-mode-syntax-table) ;; Newlines terminate comments
(modify-syntax-entry ?_ "w" neotoma-mode-syntax-table) ;; Underscores in symbols

;; Fontification (TODO)
(defun neotoma-add-font-lock ()
  "Hook to add neotoma font-lock highlighting"
  (font-lock-add-keywords
   nil
   '(
     ("\\<[A-Za-z_][A-Za-z0-9_]+\\>\s*<-" 1 'font-lock-function-name-face t)
     ("\[.*\]" 'font-lock-string-face)
     ("<-" 0 'font-lock-keyword-face)
     ("%\{" 0 'font-lock-keyword-face prepend)
     ("%\}" 0 'font-lock-keyword-face prepend)
     ("~" 0 'font-lock-builtin-face)
     ("\\[!?&]" 0 'font-lock-negation-char-face)
     )
   ))

;; (add-hook 'neotoma-mode-hook 'neotoma-add-font-lock)

;; Key bindings (TODO)

;; (define-key neotoma-mode-map)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.peg" . neotoma-mode))

(provide 'neotoma-mode)
