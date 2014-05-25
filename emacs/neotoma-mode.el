(eval-when-compile (require 'cl))
(require 'erlang)

;; Customize

(defgroup neotoma nil
  "Support for Neotoma PEG grammars"
  :group 'languages
  :prefix "neotoma-")

(defcustom neotoma-mode-hook nil
  "Hook run when entering Neotoma mode"
  :type 'hook
  :group 'neotoma)

;; Define the major mode

(define-derived-mode neotoma-mode prog-mode "Neotoma"
  "Major mode for editing Neotoma PEG grammars (Erlang).

\\{neotoma-mode-map}"
  :group 'neotoma
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'comment-start) "%% ")
  (setq indent-tabs-mode nil))

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
(font-lock-add-keywords
 'neotoma-mode
 '(
   ("\\<[A-Za-z_][A-Za-z0-9_]+\\>\s*<-" 1 font-lock-function-name-face t)
   ("\[.*\]" font-lock-string-face)
   ("<-" 0 font-lock-keyword-face)
   ("%\{" 0 font-lock-keyword-face)
   ("%\}" 0 font-lock-keyword-face)
   )
 )


;; Key bindings (TODO)

;; (define-key neotoma-mode-map)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.peg" . neotoma-mode))

(provide 'neotoma-mode)
