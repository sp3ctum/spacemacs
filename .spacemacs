;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     (colors :variables
             colors-colorize-identifiers 'all)
     syntax-checking
     org
     emacs-lisp
     git
     auto-capitalization
     markdown
     lispy
     clojure
     prodigy
     html
     (scala :variables
            scala-auto-insert-asterisk-in-comments t)
     my-scala-extensions
     yaml
     (ruby :variables
           ruby-test-runner 'rspec)
     docker
     javascript
     restclient)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(flash-region
                                      smart-dash
                                      haml-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-light
                         solarized-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Monospace"
                               :size 10
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'current
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Ensime documentation at http://ensime.github.io/editors/emacs/install/ :
  ;;
  ;; We do not recommend or support Spacemacs. We would rather that you used
  ;; stock Emacs with evil-mode. However, if you still choose to use Spacemacs,
  ;; you must add these lines to your dotspacemacs/user-init to mimic the
  ;; configuration above.
  ;;
  ;; This is only one example of where Spacemacs does everything differently,
  ;; you’re on your own for the rest. Please do not raise bug reports if you use
  ;; Spacemacs unless you can reproduce it with stock Emacs. If you would like
  ;; to change this, please create a full regression test suite running against
  ;; Spacemacs and offer to maintain it.
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)

  ;; to use the unstable development version
  (push '("melpa" . "melpa.org/packages/") configuration-layer--elpa-archives)

  (push '(ensime . "melpa-stable") package-pinned-packages))

(defun my-undo-bindings ()
  (evil-define-key 'normal global-map "-" 'goto-last-change)
  (evil-define-key 'normal global-map "+" 'goto-last-change-reverse)

  ;; they are laggy
  (setq undo-tree-visualizer-timestamps nil))

(defun my-dired-bindings ()
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "<") 'dired-up-directory))))

(defun my-work-laptop-font-size ()
  (interactive)
  (set-default-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1"))

(defun my-work-monitor-font-size ()
  (interactive)
  (set-default-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"))

(defun my-custom-normal-mode-commands ()
  ;; I liked this, but spacemacs took it away from me
  (evil-define-key 'normal global-map
    "gs" #'save-buffer
    "gb" #'helm-mini)
  (spacemacs/set-leader-keys
    "." 'helm-M-x
    "SPC" 'avy-goto-word-or-subword-1
    "y" 'avy-goto-line
    "zl" 'my-work-laptop-font-size
    "zm" 'my-work-monitor-font-size))

(defun my-windows-customizations ()
  (when (eq system-type 'windows-nt)
    (setq dotspacemacs-default-font '("Ubuntu Mono"
                                      :size 14
                                      :weight normal
                                      :width normal
                                      :powerline-scale 1.1))))

(defun my-auto-complete-bindings ()
  (global-company-mode)
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete))

(defun my-org-mode-bindings ()
  (add-hook 'org-mode-hook 'auto-fill-mode))

(defun my-projectile-config ()
  (setq projectile-use-git-grep t))

(defun my-prodigy-config ()
  (prodigy-define-service
    :name "Evil-lispy tests"
    :command "sh"
    :args '("run-tests.sh")
    :cwd "~/git/evil-lispy/"))

(defun my-clojure-config ()
  (setq clojure-enable-fancify-symbols t)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl))"))

(defmacro comment (&rest _))

(defun my-scala-config ()
  ;; workaround for this bug:
  ;; https://github.com/syl20bnr/spacemacs/issues/6578
  (with-eval-after-load 'scala-mode
    (require 'ensime))

  (add-hook 'scala-mode-hook 'smartparens-mode)

  ;; jump out of a pair of delimiters with )
  (evil-define-key 'insert smartparens-mode-map (kbd ")") 'sp-up-sexp)
  (evil-define-key 'normal scala-mode-map (kbd ")") 'sp-up-sexp)

  (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)

  ;; fix indenting this weirldy: foo.map(a => {
  ;; }
  (setq scala-indent:align-parameters nil)

  (with-eval-after-load 'ensime
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-startup-notification nil)
    (spacemacs/set-leader-keys-for-major-mode 'scala-mode
      "ä" 'my-ensime-eval-dwim
      "Ä" 'my-scala-show-repl-output
      ;; mnemonic: go to member in file
      "gm" 'helm-imenu
      "hT" 'my-ensime-insert-function-type-at-point
      "rs" 'my-scala-split-literal-string-at-point
      "ff" 'my-ensime-inf-run-scalafmt
      "nr" 'my-ensime-restart
      "br" 'my-ensime-recompile
      "bl" 'my-ensime-reload
      "bw" 'my-ensime-run-play
      "bW" 'my-ensime-stop-play
      "nf" 'my-ensime-move-region-to-own-file)

    ;; hide implicitConversion underlinings because they make it hard to see the
    ;; actual code
    ;; https://github.com/syl20bnr/spacemacs/issues/4746
    (setq ensime-sem-high-faces
          (assq-delete-all 'implicitConversion ensime-sem-high-faces)))

  ;; support for .scaml template files
  (add-to-list 'auto-mode-alist '("\\.scaml\\'" . haml-mode))
  (add-to-list 'auto-mode-alist '("\\.en\\'" . conf-unix-mode))
  (setq haml-indent-offset 2))

(defun my-ruby-config ()
  (require 'smart-dash)
  (add-hook 'ruby-mode-hook 'smart-dash-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ruby-mode)

  ;; without this, emacs will insert a comment at the start of all files. the
  ;; comment says "coding: utf-8" or similar.
  (setq ruby-insert-encoding-magic-comment nil))

(defun my-git-config ()
  ;; open new window always in a side split
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

(defun my-web-config ()
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
            (lambda ()
              (smartparens-mode 1)))

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2)))

  (define-key evil-normal-state-map (kbd "SPC ") 'company-complete)
  (spacemacs/set-leader-keys
    "å" 'my-http-run-current-call))

(defun my-http-run-current-call ()
  "Requires a http-mode buffer with point positioned at the call that should be
executed. Executes that without disrupting the frame window layout."
  (interactive)
  (let ((restclient-buffers (--filter (equal 'restclient-mode
                                             (buffer-local-value 'major-mode it))
                                      (buffer-list))))
    (if (equal (length restclient-buffers) 1)
        (with-current-buffer (-first-item restclient-buffers)
          (restclient-http-send-current-stay-in-window))
      (message "There are %s restclient buffers, but only 1 should exist. Aborting."
               (length restclient-buffers)))))

(defun my-neotree-copy-filepath-to-yank-ring ()
  (interactive)
  (neotree-copy-filepath-to-yank-ring)
  (message "copied %s" (neo-buffer--get-filename-current-line)))

(defun my-neotree-customizations ()
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key neotree-mode-map
                "y" 'my-neotree-copy-filepath-to-yank-ring))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  (global-aggressive-indent-mode)

  (my-undo-bindings)
  (my-dired-bindings)
  (my-custom-normal-mode-commands)
  (my-auto-complete-bindings)
  (my-org-mode-bindings)
  (my-projectile-config)
  (my-prodigy-config)
  (my-clojure-config)
  (my-scala-config)
  (my-web-config)
  (my-ruby-config)
  (my-git-config)
  (my-windows-customizations)
  (my-neotree-customizations))




;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
