;;; init.el --- Ian Ding's Emacs Configuration  -*- lexical-binding: t -*-

;; Author: Ian Ding <ianding0549@gmail.com>

;;; Commentary:

;; This configuration is specialized on macOS and terminal environments.

;;; Code:

;; Install and load use-package automatically
;; URL: http://cachestocaches.com/2015/8/getting-started-use-package/#auto-installing-use-package

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; Basic Emacs customizations
;; URL: https://gist.github.com/doitian/5425328
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)

(when (memq window-system '(mac ns))
  ;; Show the menubar in macOS.
  (menu-bar-mode)
  ;; Transparent title bar
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(setq enable-recursive-minibuffers t    ; Use ivy even in minibuffer
      minibuffer-depth-indicate-mode t  ; Indicate the recursive depth
      delete-by-moving-to-trash t
      tab-width 4
      indent-tabs-mode nil
      show-paren-mode t
      fill-column 78
      set-mark-command-repeat-pop t
      sentence-end-double-space nil     ; Sentence ends in one space
      help-window-select t)             ; Select the help window

(fset 'yes-or-no-p 'y-or-n-p)           ; Use y/n instead of yes/no

;; Smooth scrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10000
      scroll-preserve-screen-position t)

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq x-stretch-cursor t)

(global-auto-revert-mode 1)

;; Use eshell as the initial buffer
;;(setq initial-buffer-choice #'eshell)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

(defun load-private-config ()
  "Load the private customization file."
  (let ((private-file (concat user-emacs-directory "private.el")))
    (when (file-exists-p private-file)
      (load-file private-file))))

(add-hook 'after-init-hook #'load-private-config)

;; Window switching
(windmove-default-keybindings 'meta)

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  :bind
  (("s-1" . eyebrowse-switch-to-window-config-1)
   ("s-2" . eyebrowse-switch-to-window-config-2)
   ("s-3" . eyebrowse-switch-to-window-config-3)
   ("s-4" . eyebrowse-switch-to-window-config-4)
   ("s-5" . eyebrowse-switch-to-window-config-5)
   ("s-6" . eyebrowse-switch-to-window-config-6)
   ("s-7" . eyebrowse-switch-to-window-config-7)
   ("s-8" . eyebrowse-switch-to-window-config-8)
   ("s-9" . eyebrowse-switch-to-window-config-9)
   ("s-0" . eyebrowse-switch-to-window-config-0)
   ("s-t" . eyebrowse-create-window-config)))

;; Read PATH from the shell init script
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Disable autosave and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Ask before quitting.
(setq confirm-kill-emacs 'y-or-n-p)

(global-set-key (kbd "C-c e") #'eshell)

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init))

;; Ivy
(use-package counsel
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  :bind
  (("C-c s" . swiper)
   ("C-c r" . ivy-resume)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   :map ivy-minibuffer-map
   ("C-c C-o" . ivy-occur)))

;; Smex
(use-package smex
  :bind
  (("M-x" . smex)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package wgrep)

(use-package neotree
  :config
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-show-updir-line t
        neo-window-fixed-size nil
        neo-smart-open t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  ;; Do not wrap the lines in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil))))
  :bind (("C-c n" . neotree-toggle)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("M-S-RET" . crux-smart-open-line-above)
         ("M-RET" . crux-smart-open-line)
         ("C-^" . crux-top-join-line)
         ("C-c y" . crux-duplicate-current-line-or-region)
         ("C-c k" . crux-kill-other-buffers)
         ("C-x 4 t" . crux-transpose-windows)))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package haskell-mode
  :commands haskell-mode
  :mode "\\.hs\\'")

(use-package company
  :hook (prog-mode . company-mode))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t))

(use-package esup
  :commands (esup))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :bind
  (("C-/" . undo-tree-undo)
   ("S-z" . undo-tree-undo)
   ("S-Z" . undo-tree-redo)
   ("C-c u" . undo-tree-visualize)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package multi-term
  :config
  (setq multi-term-program "/bin/bash"))

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode))

(global-visual-line-mode)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-aggressive nil)
  (setq dumb-jump-use-visible-window t)
  :bind
  (("M-g d" . dumb-jump-go)
   ("M-g t" . dumb-jump-back)
   ("M-g q" . dumb-jump-quick-look)
   ("M-g D" . dumb-jump-go-other-window)
   ("M-g x" . dumb-jump-go-prompt)))

(use-package which-key
  :config
  (which-key-mode))

(provide 'init)
;;; init.el ends here
