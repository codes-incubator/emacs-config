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
;;      setup tool-bar, menu-bar, scroll-bar mode
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)
(toggle-scroll-bar -1)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup Emacs windows move 
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup Emacs windows rotate (ace-window)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "M-p") 'ace-window)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup Emacs buffer rotate (buffer-move)
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)
;; (setq buffer-move-behavior 'move)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup X-window sys-clipboard
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq x-select-enable-clipboard t)
(setq gui-select-enable-clipboard t)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      setup coding system and window property
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Useful global key bind
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "C-\\") 'comment-line)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Git setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
;;      NeoTree Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-to-list 'load-path "/directory/containing/neotree/")
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Go IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-mode
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    ))

(use-package company-go
  :init
  (progn
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))
    (setq company-go-show-annotation t)
    ))

(use-package go-eldoc
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ))

(use-package go-guru :defer t)

(use-package flycheck-gometalinter :defer t)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
;; (setenv "GOPATH" "/Users/deyuhua/Workspace/SourceCode/golang")
(defun go-work-on ()
  (interactive)
  (let ((path (read-directory-name "Golang PATH: ")))
    (progn
      (setenv "GOPATH" path)
      (message "Work On Golang PATH: %s" path))))

;; (defun go-get-pkg ()
;;   (interactive)
;;   (progn
;;     (let ((pkg-name (read-string "Enter GO Pkg Name:")))
;;       (start-process "gopkg" "gopkg" "go" "get" pkg-name)
;;       (switch-to-buffer "gopkg")
;;       (message "Try to install go package: " pkg-name)
;;       )))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(use-package go-autocomplete)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun pkg-go-mode-hook ()
  "Use goimports instead of go-fmt."
  (setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
  (local-set-key (kbd "M-l") 'godef-jump)
  (local-set-key (kbd "M-h") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'pkg-go-mode-hook)

;;      Golang Playgraound for emacs
(use-package go-playground)
(global-set-key (kbd "M-<RET>") 'go-playground-exec)

;;      Dlv Debugger
(require 'go-dlv)

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

(require 'yaml-mode)
   (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;      Python IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq python-indent-offset 4)
(setenv "PYTHONIOENCODING" "utf-8")

(use-package pyvenv
  :config
  (progn
    (pyvenv-mode)
    (defalias 'workon 'pyvenv-workon)))

(use-package elpy
  :init
  (progn
    (elpy-enable)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    ;; enable elpy jedi backend
    (setq elpy-rpc-backend "jedi")
    ))

(use-package cython-mode :defer t)
(use-package yapfify
  :init
  (progn
    (add-hook 'python-mode-hook 'yapf-mode)
    ))

(use-package anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    ))

(use-package company-anaconda
  :init
  (progn
    (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))
    (add-hook 'python-mode-hook 'anaconda-mode)
    ))

(defun pkg-disable-multi-auto-complete ()
  (auto-complete-mode -1))
(add-hook 'python-mode-hook 'pkg-disable-multi-auto-complete)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window buffer-move neotree company-anaconda anaconda-mode yapfify cython-mode elpy pyvenv markdown-mode yaml-mode use-package magit go-playground go-guru go-eldoc go-dlv go-autocomplete git-gutter+ flycheck-gometalinter company-go))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
