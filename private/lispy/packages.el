(defconst lispy-packages
  '(lispy
    (evil-lispy :location (recipe :fetcher github
                                  :repo "sp3ctum/evil-lispy"
                                  :branch "master"))))
(defun lispy/init-lispy ()
  (use-package lispy
    :defer t))

(defun lispy/init-evil-lispy ()
  (use-package evil-lispy
    :init (add-hook emacs-lisp-mode-hook #'evil-lispy-mode)
    :defer t))

;; todo call spacemacs|add-toggle
;; (spacemacs|add-toggle evil-cleverparens
;;   :status evil-cleverparens-mode
;;   :on  (evil-cleverparens-mode)
;;   :off (evil-cleverparens-mode -1)
;;   :documentation "Enable evil-cleverparens.")
