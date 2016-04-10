(defconst lispy-packages
  '(lispy
    (evil-lispy :location (recipe :fetcher github
                                  :repo "sp3ctum/evil-lispy"
                                  :branch "master"))))

(defun lispy/init-lispy ()
  (use-package lispy
    :defer t
    :config (spacemacs|diminish lispy-mode "" "")))

(defun lispy/init-evil-lispy ()
  (use-package evil-lispy
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
    :commands (evil-lispy-mode)
    :config (progn
              (spacemacs|diminish evil-lispy-mode " Ⓛ" " L")

              (when (configuration-layer/package-usedp 'cider)

                ;; todo better mechanism of loading cider
                (require 'cider)
                ;; show eval results in a cider overlay, next to point
                (add-to-list 'lispy-compat 'cider)
                (setq lispy-eval-display-style 'overlay))

              (define-key lispy-mode-map "o" 'special-lispy-different)
              (define-key lispy-mode-map "d" 'special-lispy-other-mode)
              (define-key lispy-mode-map "i" 'special-lispy-flow)
              (define-key lispy-mode-map "f" 'special-lispy-tab))))
