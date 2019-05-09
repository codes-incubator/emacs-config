;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Setup pkg repo and install use-package
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'package)
(setq package-enable-at-startup nil)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup coding system and window property
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; setup titlebar appearance
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; useful mode settings
(display-time-mode 1)
(column-number-mode 1)
(global-hl-line-mode t)
(show-paren-mode nil)
(display-battery-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(global-auto-revert-mode t)
(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(toggle-frame-fullscreen)

;; file edit settings
(setq tab-width 4
      inhibit-splash-screen t
      initial-scratch-message nil
      sentence-end-double-space nil
      make-backup-files nil
      indent-tabs-mode nil
      make-backup-files nil
      auto-save-default nil)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup history of edited file
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/.savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      misc settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq custom-file "~/.emacs.d/.custom.el")
(load custom-file t)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Automatically compile Emacs Lisp libraries
;;      https://github.com/emacscollective/auto-compile
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package auto-compile
  :init (setq load-prefer-newer t)
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Show key bind for currently entered incomplete command
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package which-key
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Show recent file
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package recentf
  :config
  (progn
    (setq recentf-max-saved-items 200
	  recentf-max-menu-items 15)
    (recentf-mode)
    ))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Show line number of current coding window
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package linum
  :init
  (progn
    (global-display-line-numbers-mode nil)
    (setq display-line-numbers "%4d \u2502")
    ))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Auto pair when input
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package autopair
  :config (autopair-global-mode))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Sidebar for emacs
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package all-the-icons)
(use-package neotree
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neo-window-fixed-size nil)
    (setq-default neo-show-hidden-files t)
    (global-set-key [f9] 'neotree-refresh)
    (global-set-key [f10] 'neotree-toggle)))

(use-package magit
  :config
  (progn
    (global-set-key (kbd "C-x g") 'magit-status)
    ))


(use-package git-gutter+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Emacs framework for incremental completions and
;;      narrowing selections
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package helm-swoop)
(use-package helm-gtags)
(use-package helm
  :diminish helm-mode
  :init
  (progn
    ;; (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
	  helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
	  helm-yas-display-key-on-candidate t
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-ff-skip-boring-files t)
    (helm-mode))
  :config
  (progn
    )
  :bind  (("C-i" . helm-swoop)
	  ("C-x C-f" . helm-find-files)
	  ("C-x b" . helm-buffers-list)
	  ("M-y" . helm-show-kill-ring)
	  ("M-x" . helm-M-x)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Snippet tool for emacs
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    ;; (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands nil)
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))


(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
	   (member major-mode
		   '(emacs-lisp-mode
		     lisp-mode
		     clojure-mode
		     scheme-mode
		     haskell-mode
		     ruby-mode
		     rspec-mode
		     python-mode
		     c-mode
		     c++-mode
		     objc-mode
		     latex-mode
		     js-mode
		     plain-tex-mode))
	   (let ((mark-even-if-inactive transient-mark-mode))
	     (indent-region (region-beginning) (region-end) nil))))))

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
	(setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
	(setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
	(setq tmpsource
	      (list
	       (cons 'name prompt)
	       (cons 'candidates cands)
	       '(action . (("Expand" . (lambda (selection) selection))))
	       ))
	(setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
	(if (null result)
	    (signal 'quit "user quit!")
	  (cdr (assoc result rmap))))
    nil))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Markdown mode setting
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package markdown-mode
  :ensure t
  :bind (("C-c p" . livedown-preview)
	 ("C-c k" . livedown-kill))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
