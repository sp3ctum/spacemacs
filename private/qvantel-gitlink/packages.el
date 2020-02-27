;;; packages.el --- qvantel-gitlink layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Mika Vilpas <mika.vilpas@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:

(defconst qvantel-gitlink-packages
  '(git-link)
  "The list of Lisp packages required by the qvantel-gitlink layer.")


(defun my-git-link--push-remote (branch)
  (git-link--get-config (format "branch.%s.pushRemote" branch)))

(defun my-git-link-get-qvantel-stash-viewurl ()
  (let* ((view-url (git-link--get-config
                    (s-join ""
                            (list "remote."
                                  (or (my-git-link--push-remote (git-link--branch))
                                      (git-link--remote))
                                  ".viewurl")))))
    (when (not view-url)
      (user-error "Missing viewurl base. set remote.foo.viewurl to e.g.
                   https://stash.qvantel.net/projects/API/repos/bssapi-entities/ and try
                   again. (needs to have a trailing slash)"))
    view-url))

(defun my-git-link-qvantel-stash (hostname dirname filename branch commit start end)
  (format "%sbrowse/%s?at=refs%%2Fheads%%2F%s#%s"
          (my-git-link-get-qvantel-stash-viewurl)
          filename
          branch
          start))

(defun my-git-link-commit-qvantel-stash (hostname dirname commit)
  (format "%scommits/%s"
          (my-git-link-get-qvantel-stash-viewurl)
          commit))

(defun qvantel-gitlink/post-init-git-link ()
  (with-eval-after-load 'git-link
    (when (configuration-layer/package-usedp 'git-link)
      (add-to-list 'git-link-remote-alist '("qvantel" my-git-link-qvantel-stash))
      (add-to-list 'git-link-commit-remote-alist '("qvantel" my-git-link-commit-qvantel-stash)))))

;;; packages.el ends here
