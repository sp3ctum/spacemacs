;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-scala-extensions-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-scala-extensions/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-scala-extensions/pre-init-PACKAGE' and/or
;;   `my-scala-extensions/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-scala-extensions-packages
  '(scala-mode smartparens ensime)
  "The list of Lisp packages required by the my-scala-extensions layer.

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


(defun my-evil-flash-region ()
  (require 'flash-region)
  (evil-normal-state t)

  (let ((face (defface my-ensime-eval-region-face
                `((t :inherit 'default
                     :background "light sea green"))
                "lispy state face."
                :group 'spacemacs)))
    (flash-region start end face 0.1))
  (evil-visual-restore)
  (evil-normal-state t))

(defun my-prefix-lines (prefix multiline-text)
  (->> (s-lines multiline-text)
       (--filter (not (s-blank? it)))
       (--map (s-trim-left (s-prepend prefix it)))
       (s-join "\n")
       (s-append "\n")))

(defun my-scala-get-repl-output ()
  ;; if no result has yet arrived, will signal an error.
  ;; (ensime-inf-eval-result) may also return the empty string
  (ignore-errors
    (s-trim (ensime-inf-eval-result))))

(defun my-scala-wait-for-repl-output ()
  ;; having save-window-excursion and save-excursion are required, lest the
  ;; current window change position. this is a drawback in emacs's ensime
  ;; implementation.
  ;; todo things that don't print anything will timeout
  (save-window-excursion
    (save-excursion
      (with-timeout (5 (message "scala repl evaluation timed out."))
        ;; only get the result once for performance
        (let (result)
          (while (s-blank? (setq result (my-scala-get-repl-output)))
            (sit-for 0.05))
          result)))))

(defun my-scala-clear-repl-buffer ()
  (with-current-buffer ensime-inf-buffer-name
    ;; if the buffer gets too large (a few kilobytes?), performance will suffer
    (comint-clear-buffer)))

(defun my-scala-show-repl-output ()
  "Sometimes the repl output is displayed only partially. Use this to show the
last output as it exists right now."
  (interactive)
  (let ((eval-result (my-scala-wait-for-repl-output)))
    (when (not (s-blank? eval-result))
      (kill-new (my-prefix-lines "// " eval-result)))
    eval-result))

(defun my-ensime-eval-dwim (start end)
  (interactive "r")
  (require 'popup)

  (my-scala-clear-repl-buffer)
  (my-evil-flash-region)
  (ensime-inf-eval-region start end)
  (my-scala-show-repl-output))

(defun my-ensime-print-type-at-point ()
  (interactive)
  (ensime-type-at-point '(4) nil))

(defun my-ensime-print-type-at-point-full-name ()
  (interactive)
  (ensime-type-at-point '(4) t))

(defun my-ensime-insert-function-type-at-point ()
  "point must be on the line where the function starts"
  (interactive)
  (save-excursion
    (let ((type (ensime-print-type-at-point t)))
      (re-search-forward "(")
      (sp-end-of-sexp)
      (forward-char 1)
      (insert ": " type))))

(defun my-scala-split-literal-string-at-point ()
  (interactive)
  (save-excursion
    (insert "\" + \n\"")
    (indent-region (line-beginning-position)
                   (line-beginning-position 2))))

(defun my-ensime-inf-run-scalafmt ()
  (interactive)
  ;; custom formatting command
  (sbt-command "fmt")
  (message "running scalafmt..."))

(defun my-ensime-recompile ()
  (interactive)
  (ensime-sbt-do-clean)
  (ensime-sbt-do-compile)
  (message "Recompiling..."))

(defun my-ensime-reload ()
  (interactive)
  (sbt-command "reload"))

(defun my-ensime-run-play ()
  (interactive)
  (sbt-command "run")
  (message "starting play with 'sbt run'"))

(defun my-ensime-stop-play ()
  (interactive)
  (with-current-buffer (sbt:buffer-name)
    (comint-send-eof))
  (message "stopping play"))

(defun my-ensime-restart ()
  "Restarts the ensime server. Sometimes the server just starts
hanging. it's quite fast to restart, but it's a lot of headwork.
Better to automate it with something like this."
  (interactive)
  (ensime-shutdown)
  (ensime))

(defun my-ensime-get-packages-and-imports ()
  (-take-while (lambda (line)
                 (or (s-blank? line)
                     (s-matches? "package" line)
                     (s-matches? "import" line)))
               (s-lines (buffer-substring-no-properties 1 (point-max)))))

(defun my-ensime-move-region-to-own-file (beg end)
  (interactive "r")
  (let* ((filename (read-file-name "Move region to file: "))
         (text (delete-and-extract-region beg end))
         (packages-and-imports (my-ensime-get-packages-and-imports))
         (new-file (find-file-other-window filename)))
    (switch-to-buffer new-file)
    (--map (progn
             (insert it)
             (insert "\n"))
           packages-and-imports)
    (insert text)))


(defun my-ensime-test-file-name ()
  (->> (f-this-file)
       (s-replace "/src/main/" "/src/test/")))

(defun my-ensime-switch-to-test-file ()
  (interactive)
  (let* ((packages-and-imports (my-ensime-get-packages-and-imports))
         (test-file-name (->> (my-ensime-test-file-name)
                              (s-replace ".scala" "Spec.scala")))
         (new-file (find-file-other-window test-file-name)))
    (switch-to-buffer new-file)
    (--map (insert it "\n") packages-and-imports)
    (message "Switched to test file at %s" (f-this-file))))

(defun my-ensime-switch-between-test-and-implementation ()
  (interactive)
  ;; TODO
  )

;;; packages.el ends here
