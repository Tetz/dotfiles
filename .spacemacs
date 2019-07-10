;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     php
     docker
     ruby
     (go :variables
         go-use-gometalinter t
         go-tab-width 4
         gofmt-command "goimports")
     python
     c-c++
     themes-megapack
     osx
     sql
     yaml
     markdown
     html
     javascript
     react
     scala
     swift
     colors
     helm
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     syntax-checking
     version-control
     )
   dotspacemacs-additional-packages '(
     helm-ls-git
     eslint-fix
     all-the-icons
     swift-mode
     vue-mode
     terraform-mode
     solidity-mode
     rjsx-mode
     company-sourcekit
   )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   ;; dotspacemacs-themes '(sanityinc-tomorrow-bright)
   dotspacemacs-themes '(ujelly)
   ;; dotspacemacs-themes '(tango-2)
   ;; dotspacemacs-themes '(grandshell)
   ;; dotspacemacs-themes '(wombat)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '("ensime" . "melpa-stable") package-pinned-packages)
)

(defun dotspacemacs/user-config ()
  ;; Shortcuts
  (spacemacs/set-leader-keys "fi" 'helm-ls-git-ls)
  (spacemacs/set-leader-keys "gp" 'grep-find)

  ;; PowerLine Separator
  (setq powerline-default-separator 'slant)

  ;; Auto-completion
  (global-company-mode t)
  (company-mode t)

  ;; iOS
  (require 'company-sourcekit)
  (add-to-list 'company-backends 'company-sourcekit)

  ;; Icon
  (require 'all-the-icons)
  (setq neo-theme 'icons)

  ;; linum-mode performance hack: http://d.hatena.ne.jp/daimatz/20120215/1329248780
  (setq linum-delay t)
  (defadvice linum-schedule (around my-linum-schedule () activate)
    (run-with-idle-timer 0.2 nil #'linum-update-current))

  ;; Indent
  (setq-default js2-strict-missing-semi-warning nil
                js2-basic-offset 2
                js-indent-level 2)

  ;; Eslint
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
      ))
  ;; use web-mode for .jsx files
  (add-to-list 'auto-mode-alist '("\\.js$" . react-mode))

  ;; Web-mode color
;;  (custom-set-faces
;;   '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
;;   '(web-mode-html-tag-face          ((t (:foreground "#55efc4"))))
;;   '(web-mode-html-tag-bracket-face  ((t (:foreground "#dfe6e9"))))
;;   '(web-mode-html-attr-name-face    ((t (:foreground "#ffeaa7"))))
;;   '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
;;   '(web-mode-html-attr-value-face   ((t (:foreground "#fd79a8"))))
;;   '(web-mode-comment-face           ((t (:foreground "#74b9ff"))))
;;   '(web-mode-server-comment-face    ((t (:foreground "#74b9ff"))))
;;
;;   '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
;;   '(web-mode-comment-face           ((t (:foreground "#74b9ff"))))
;;   '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
;;   '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
;;   '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
;;   '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
;;   )

  ;; turn on flychecking globally
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
   (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

  ;; https://github.com/purcell/exec-path-from-shell
  ;; only need exec-path-from-shell on OSX
  ;; this hopefully sets up path and other vars better
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; adjust indents for web-mode to 2 spaces
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;; for better jsx syntax-highlighting in web-mode
  ;; - courtesy of Patrick @halbtuerke
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (defun my/npm-command(command)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-command (executable-find command))
           (local-command (expand-file-name (concat "node_modules/.bin/" command) root)))

      (cond ((file-executable-p local-command) local-command)
            (else global-command))))

  (defun my/prefer-local-eslint ()
    (let ((eslint (my/npm-command "eslint")))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  (add-hook 'js2-mode-hook 'my/prefer-local-eslint)

  (defun eslint-fix ()
    (interactive)
    (let ((eslint (my/npm-command "eslint")))
      (progn (call-process eslint nil "*ESLint Errors*" nil "--fix" buffer-file-name)
             (revert-buffer t t t))))
  (provide 'eslint-fix)

  ;; Start ensime automatically and hide ensime's notification
  (setq imenu-auto-rescan t)
  (add-hook 'scala-mode-hook 'ensime)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)

  ;; scalastyle
  (setq-default flycheck-scalastylerc "scalastyle_config.xml")

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("439d4ce8295685fc36fc119a062d0283bb05436ae841b053af76e9a5e42bc670" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#3a3a3a" t)
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(hl-sexp-background-color "#121212")
 '(package-selected-package
   (quote
    (rainbow-mode rainbow-identifiers treepy graphql color-identifiers-mode zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme xpm company-sourcekit sourcekit rjsx-mode ghub let-alist solidity-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby flycheck-gometalinter memoize go-guru go-eldoc company-go go-mode swift-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl terraform-mode hcl-mode all-the-icons font-lock+ sql-indent eslintd-fix eslint-fix unfill smeargle orgit mwim magit-gitflow helm-gitignore helm-company helm-c-yasnippet gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor diff-hl company-web web-completion-data company-tern dash-functional company-statistics auto-yasnippet auto-dictionary ac-ispell auto-complete tern yaml-mode mmm-mode markdown-toc markdown-mode gh-md web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode helm-ls-git noflet ensime company sbt-mode scala-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode winum ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(package-selected-packages
   (quote
    (drupal-mode phpunit phpcbf php-extras php-auto-yasnippets php-mode vue-mode edit-indirect ssass-mode vue-html-mode disaster company-c-headers cmake-mode clang-format dockerfile-mode docker tablist docker-tramp zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode ws-butler winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org terraform-mode tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit swift-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spaceline spacegray-theme soothe-theme solidity-mode solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reverse-theme reveal-in-osx-finder restart-emacs rebecca-theme rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pbcopy paradox osx-trash osx-dictionary orgit organic-green-theme org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noflet noctilux-theme neotree naquadah-theme mwim mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme less-css-mode launchctl json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-ls-git helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio go-guru go-eldoc gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy flycheck-pos-tip flycheck-gometalinter flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eslint-fix ensime emmet-mode elisp-slime-nav dumb-jump dracula-theme django-theme diminish diff-hl darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web company-tern company-statistics company-sourcekit company-go company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme all-the-icons alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#000000" :family "Hack" :foundry "nil" :slant normal :weight normal :height 141 :width normal))))
 '(web-mode-comment-face ((t (:foreground "#74b9ff"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#ffeaa7"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#fd79a8"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#dfe6e9"))))
 '(web-mode-html-tag-face ((t (:foreground "#55efc4"))))
 '(web-mode-server-comment-face ((t (:foreground "#74b9ff")))))

