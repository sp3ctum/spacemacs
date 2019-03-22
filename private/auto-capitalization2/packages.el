;;; packages.el --- auto-capitalization2 layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mika Vilpas <mvilpas@QFINL014>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `auto-capitalization2-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `auto-capitalization2/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `auto-capitalization2/pre-init-PACKAGE' and/or
;;   `auto-capitalization2/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst auto-capitalization2-packages
  '((auto-capitalize :location
                     (recipe :repo "sp3ctum/auto-capitalize-el"
                             :fetcher github)))
  "The list of Lisp packages required by the auto-capitalization2 layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun auto-capitalization2/init-auto-capitalize ()
  (use-package auto-capitalize
    :init (add-hook 'text-mode-hook #'turn-on-auto-capitalize-mode)
    :config (setq auto-capitalize-words
                  (list "I" "I'd" "I've"))))

(defvar auto-capitalization2/ignore-words
  (list "e.g." "i.e." "vs.")
  "These words will always appear as they are, and any following
  word will not be capitalized.")


;;; packages.el ends here
