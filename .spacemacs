;; -*- mode: emacs-lisp -*-


(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   ;; ----------------------------------------------------------------
   ;; <M-m f e R> (Emacs style) to install them.
   ;; ----------------------------------------------------------------
   dotspacemacs-configuration-layers
   '(
     csv
     typescript
     vimscript
     docker
     git
     go
     ruby
     (python :variables
             python-backend 'lsp python-lsp-server 'pyright
             python-format-on-save t)
     c-c++
     themes-megapack
     osx
     sql
     yaml
     markdown
     html
     javascript
     react
     ;;php
     scala
     swift
     colors
     helm
     auto-completion
     better-defaults
     emacs-lisp
     markdown
     syntax-checking
     version-control
     neotree
     prettier
   )
   dotspacemacs-additional-packages '(
     mozc
     helm-ls-git
     eslint-fix
     all-the-icons
     vue-mode
     solidity-mode
     rjsx-mode
     go-mode
     company-sourcekit
     prettier-js
   )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (
   setq-default
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
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(ujelly
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
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
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ())
(defun dotspacemacs/user-config ()
  ;; Shortcuts
  (spacemacs/set-leader-keys "fi" 'helm-ls-git-ls)
  (spacemacs/set-leader-keys "gp" 'grep-find)
  (spacemacs/set-leader-keys "-" 'split-window-vertically)
  (spacemacs/set-leader-keys "\\" 'split-window-horizontally)

  ;; PowerLine Separator
  (setq powerline-default-separator 'arrow)

  ;; Mozc settings
  (require 'mozc)  ; or (load-file "/path/to/mozc.el")
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay)

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

  ;; Go-lang
  (setq go-use-gometalinter t)
  (setq go-backend 'lsp)
  (setq go-format-before-save t)
  (setq go-tab-width 4)
  (setq go-use-test-args "-race -timeout 10s")
  (setq godoc-at-point-function 'godoc-gogetdoc)

  ;; Javascript
  (setq-default
   ;; js-mode and js2-mode
   js-indent-level 2
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . web-mode))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.[c|m]js[x]?\\'")))

  ;; Typescript
  (setq-default typescript-indent-level 2)
  (setq typescript-fmt-on-save t)
  (setq typescript-fmt-tool 'prettier)
  (setq typescript-linter 'eslint)
  ;; Enable React syntax highlighting for .tsx files
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  ;; Javascript auto-fix
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-js nil t))))

  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-js nil t))))

  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-js nil t))))

  (eval-after-load 'react-mode
    '(add-hook 'react-mode-hook (lambda () (add-hook 'after-save-hook 'prettier-js nil t))))

)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (mozc csv-mode prettier-js tide typescript-mode vimrc-mode dactyl-mode flycheck-gometalinter go-guru go-eldoc company-go go-mode zenburn-theme zen-and-art-theme yapfify yaml-mode white-sand-theme web-mode web-beautify vue-mode edit-indirect ssass-mode vue-html-mode unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit swift-mode sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacegray-theme soothe-theme solidity-mode solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode seti-theme scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode reverse-theme reveal-in-osx-finder rebecca-theme rbenv rake rainbow-mode rainbow-identifiers railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme planet-theme pip-requirements phpunit phpcbf php-extras php-auto-yasnippets phoenix-dark-pink-theme phoenix-dark-mono-theme pbcopy osx-trash osx-dictionary orgit organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noflet noctilux-theme naquadah-theme mwim mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow madhat2r-theme lush-theme livid-mode skewer-mode simple-httpd live-py-mode light-soap-theme less-css-mode launchctl js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme hy-mode heroku-theme hemisu-theme helm-pydoc helm-ls-git helm-gitignore helm-css-scss helm-company helm-c-yasnippet hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md gandalf-theme fuzzy flycheck-pos-tip pos-tip flycheck flatui-theme flatland-theme farmhouse-theme exotica-theme evil-magit magit transient git-commit with-editor espresso-theme eslint-fix ensime sbt-mode scala-mode emmet-mode drupal-mode php-mode dracula-theme dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat django-theme disaster diff-hl darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-web web-completion-data company-tern tern company-statistics company-sourcekit sourcekit dash-functional company-c-headers company-anaconda company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode cmake-mode clues-theme clang-format chruby cherry-blossom-theme busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme all-the-icons memoize alect-themes afternoon-theme ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#000000")))))
