(defconst auto-capitalization-packages '(auto-capitalize))

;; todo
(defvar auto-capitalization/ignore-words
  (list "e.g." "i.e." "vs.")
  "These words will always appear as they are, and any following
  word will not be capitalized.")

(defun auto-capitalization/init-auto-capitalize ()
  (use-package auto-capitalize
    :init (add-hook 'text-mode-hook #'turn-on-auto-capitalize-mode)
    :config (setq auto-capitalize-words
                  (list "I" "I'd" "I've"))))
