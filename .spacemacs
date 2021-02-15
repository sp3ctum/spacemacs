;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d/private/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(react
     nginx
     typescript
     c-c++
     rust
     (org :variables org-enable-reveal-js-support t)
     lsp
     dap                                ;debugger
     python
     csv
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     (colors :variables
             colors-colorize-identifiers 'all)
     helm
     syntax-checking
     (org :variables
          org-enable-jira-support t)
     emacs-lisp
     git
     auto-capitalization2
     markdown
     lispy
     qvantel-gitlink
     clojure
     prodigy
     html
     yaml
     (ruby :variables
           ruby-test-runner 'rspec)
     (scala :variables
            scala-backend 'scala-metals)
     docker
     groovy
     javascript
     restclient
     copy-as-format)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;;
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(flash-region
                                      prettier
                                      nvm
                                      smart-dash
                                      haml-mode
                                      dtrt-indent
                                      coffee-mode)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   dotspacemacs-mode-line-theme 'spacemacs
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
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

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))



(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

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
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))

(defun my-custom-normal-mode-commands ()
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq create-lockfiles nil)

  ;; I liked this, but spacemacs took it away from me
  (evil-define-key 'normal global-map
    "gs" #'save-buffer
    "gb" #'helm-mini)

  (setq dotspacemacs-command-key (kbd "."))
  (spacemacs/set-leader-keys
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
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
  (setq org-todo-keywords '((sequence "TODO" "ONGOING" "DONE")))
  (setq org-todo-keyword-faces '(("ONGOING" . "orange")
                                 ("DONE" . "lawngreen")))
  (setq org-export-copy-to-kill-ring 'if-interactive)
  (setq org-directory "~/git/org/")
  (setq org-default-notes-file "active.org"))

(defun my-projectile-config ()
  ;; Working with compilation buffers is annoying because any newly generated
  ;; window will steal point. Fix that. After this, all new compilation windows
  ;; will not get the focus.
  (defun my-saving-current-window (old-function &rest arguments)
    (save-selected-window
      (apply old-function arguments)))

  (advice-add #'compilation-start :around #'my-saving-current-window)
  ;; Don't try to look for TAGS files when g d (go to definition) doesn't find
  ;; anything. this is annoying as I don't want to use TAGS tables.
  (remove-hook 'xref-backend-functions 'etags--xref-backend)
  (setq projectile-use-git-grep t))

(defun my-clojure-config ()
  (setq clojure-enable-fancify-symbols t))

(defmacro comment (&rest _))

(defun my-scala-config ()
  (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)

  ;; fix indenting this weirldy: foo.map(a => {
  ;; }
  (setq scala-indent:align-parameters nil)

  (spacemacs/set-leader-keys-for-major-mode 'scala-mode
    "Ä" 'my-scala-show-repl-output
    ;; mnemonic: go to member in file
    "gm" 'helm-imenu
    "rs" 'my-scala-split-literal-string-at-point
    )

  ;; support for .scaml template files
  (add-to-list 'auto-mode-alist '("\\.scaml\\'" . haml-mode))
  (add-to-list 'auto-mode-alist '("\\.en\\'" . conf-unix-mode))
  (setq haml-indent-offset 2))

(defun my-ruby-config ()

  (with-eval-after-load 'haml-mode
    (add-hook 'haml-mode-hook 'dtrt-indent-mode)
    (add-hook 'haml-mode-hook (lambda ()
                                (interactive)
                                (aggressive-indent-mode nil))))
  (add-to-list 'aggressive-indent-excluded-modes 'ruby-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'coffee-mode)

  ;; without this, emacs will insert a comment at the start of all files. the
  ;; comment says "coding: utf-8" or similar.
  (setq ruby-insert-encoding-magic-comment nil)
  (setq lsp-solargraph-use-bundler t)

  (add-hook 'ruby-mode-hook
            (lambda ()
              (lsp)
              (lsp-ui-mode))))

(defun my-git-config ()
  ;; open new window always in a side split
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

(defun my-web-config ()
  (setq web-mode-markup-indent-offset 2)
  (setq javascript-backend 'lsp)

  (nvm-use "10.13")
  ;; https://github.com/emacs-lsp/lsp-mode/issues/666
  (add-to-list 'exec-path "/home/mvilpas/.nvm/versions/node/v10.15.3/bin")

  (add-to-list 'aggressive-indent-excluded-modes 'js2-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (prettier-js-mode)
              (smartparens-mode 1)
              (dtrt-indent-mode)))

  (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-assume-strict t)
  (add-hook 'css-mode-hook (lambda ()
                             (prettier-js-mode)))
  (add-hook 'js2-mode-hook
            (defun my-js2-mode-hook ()
              (interactive)
              (prettier-js-mode)
              (dtrt-indent-mode)
              (spacemacs/toggle-rainbow-identifier-off)))

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
  (setq treemacs-width 50)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key neotree-mode-map
                "y" 'my-neotree-copy-filepath-to-yank-ring))))

(defun my-python-config ()

  (add-to-list 'exec-path "/home/mvilpas/.pyenv/plugins/pyenv-virtualenv/shims")
  (add-to-list 'exec-path "/home/mvilpas/.pyenv/shims")
  (add-to-list 'exec-path "/home/mvilpas/.pyenv/bin")

  (setq python-shell-interpreter "python3")
  (setq python-pipenv-activate t)
  (setq python-backend 'lsp)

  ;; https://github.com/proofit404/pyenv-mode#projectile-integration
  ;; (require 'pyenv-mode)

  ;; (defun projectile-pyenv-mode-set ()
  ;;   "Set pyenv version matching project name."
  ;;   (let ((project (projectile-project-name)))
  ;;     (if (--any (s-contains? it project)
  ;;                (pyenv-mode-versions))
  ;;         (progn
  ;;           (message "Using pyenv %s" project)
  ;;           (pyenv-mode-set project))
  ;;       (progn
  ;;         (message "Unknown project %s. Unsetting pyenv." project)
  ;;         (pyenv-mode-unset)))))

  ;; (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook (lambda ()
                                (lsp)
                                (lsp-ui-mode t)))
  (with-eval-after-load 'python-mode))

(defun fix-expand-region ()
  (defun org-outline-overlay-data (&rest _))
  (defun org-set-outline-overlay-data (&rest _)))

(defun my-lsp-config ()
  (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    "-" 'lsp-format-buffer
    "m" 'lsp-ui-imenu))

(defun my-docker-customizations ()
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (setq-local indent-line-function #'sh-indent-line))))

(defun my-java-config ()
  (add-hook 'java-mode-hook (lambda ()
                              (add-to-list 'aggressive-indent-excluded-modes
                                           'java-mode))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  (global-aggressive-indent-mode)

  (my-lsp-config)
  (my-undo-bindings)
  (my-dired-bindings)
  (my-custom-normal-mode-commands)
  (my-auto-complete-bindings)
  (my-org-mode-bindings)
  (my-projectile-config)
  (my-python-config)
  (my-clojure-config)
  (my-scala-config)
  (my-web-config)
  (my-ruby-config)
  (my-git-config)
  (my-windows-customizations)
  (my-neotree-customizations)
  (fix-expand-region)
  (my-docker-customizations)
  (my-java-config))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

;; ;; Do not write anything past this comment. This is where Emacs will
;; ;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(c-default-style (quote ((java-mode . "java"))))
   '(lsp-java-format-enabled nil)
   '(lsp-ui-doc-delay 999)
   '(lsp-ui-sideline-show-code-actions nil)
   '(org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
   '(org-fontify-quote-and-verse-blocks t)
   '(package-selected-packages
     (quote
      (tern rjsx-mode prettier iter2 org-re-reveal nginx-mode nvm lsp-metals tide typescript-mode import-js grizzl add-node-modules-path toml-mode racer helm-gtags ggtags flycheck-rust counsel-gtags cargo rust-mode dap-mode bui tree-mode copy-as-format lsp-mode zenburn-theme yasnippet-snippets yapfify yaml-mode ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org tagedit symon string-inflection spaceline-all-the-icons smeargle smart-dash slim-mode seeing-is-believing scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe restclient-helm restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters pytest pyenv-mode py-isort pug-mode prodigy prettier-js popwin pippel pipenv pip-requirements persp-mode password-generator paradox overseer orgit org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file ob-restclient ob-http noflet nameless mvn move-text mmm-mode minitest meghanada maven-test-mode markdown-toc magit-svn magit-gitflow macrostep lsp-ui lsp-java lorem-ipsum livid-mode live-py-mode link-hint json-navigator js2-refactor js-doc indent-guide importmagic impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag groovy-mode groovy-imports gradle-mode google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flycheck-pos-tip flx-ido flash-region fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lispy evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu ensime emmet-mode elisp-slime-nav editorconfig dumb-jump dtrt-indent dotenv-mode doom-modeline dockerfile-mode docker diminish define-word cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-restclient company-lsp company-emacs-eclim company-anaconda column-enforce-mode color-identifiers-mode coffee-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu cider chruby centered-cursor-mode bundler auto-yasnippet auto-highlight-symbol auto-compile auto-capitalize aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
   '(safe-local-variable-values
     (quote
      ((eval progn
             (when
                 (not nvm-current-version)
               (nvm-use "10.13"))
             (when
                 (not rvm--current-ruby)
               (rvm-activate-corresponding-ruby)))
       (eval progn
             (nvm-use "10.13")
             (rvm-activate-corresponding-ruby))
       (python-backend . lsp)
       (javascript-backend . lsp)
       (helm-projectile-git-grep-command . "git --no-pager grep --no-color --recurse-submodules -n%c -e %p -- %f")))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(lsp-ui-sideline-global ((t (:box (:line-width 2 :color "gray11" :style pressed-button) :height 0.8))))
   '(org-quote ((t (:foreground "rosy brown" :height 0.9 :family "Ubuntu Mono")))))
  )
