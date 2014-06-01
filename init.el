;; Require Emacs' package functionality
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;----------------------------------------------------------------------
; Full screen usage

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;----------------------------------------------------------------------
; Haskell Mode (against clone of git repository)

(load "~/.emacs.d/repos/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;----------------------------------------------------------------------
; Org present mode

(add-to-list 'load-path "~/.emacs.d/repos/org-present")
(require 'org-present)

;----------------------------------------------------------------------
; key-bindings

(global-unset-key "\C-x@")
(global-set-key "\C-x@" 'compile)
(global-unset-key "\C-c\C-c")
(global-set-key "\C-c\C-c" 'comment-region)

;----------------------------------------------------------------------
; Modes for various file types

(add-to-list 'auto-mode-alist '("[.]ino$" . c++-mode))
(add-to-list 'auto-mode-alist '("[.]pde$" . c++-mode))
(add-to-list 'auto-mode-alist '("[.]h$" . c++-mode))

;----------------------------------------------------------------------
; A customised mode for C++
;     - no indentation within namespaces

(defconst twd-cc-style
  '( "bsd"
     (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "twd" twd-cc-style)

;----------------------------------------------------------------------
; General settings

; Use uniquify to give better buffer names for identical file names in
; different directories.  (See customized uniquify-buffer-name-style
; above)
(require 'uniquify)

; Don't use tabs at all
(setq-default indent-tabs-mode nil)

; Dynamic abbreviations don't fiddle with case
(setq-default dabbrev-case-replace nil)
(setq-default dabbrev-case-fold-search nil)

(put 'narrow-to-region 'disabled nil)

(setq inhibit-startup-message t)

;----------------------------------------------------------------------
; projectile configuration
; 
(require 'projectile)
(projectile-global-mode)
(require 'flx-ido)
(flx-ido-mode 1)

;----------------------------------------------------------------------
; Customisation

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "twd") (c++-mode . "twd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(font-use-system-font t)
 '(projectile-globally-ignored-directories (quote (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" ".cabal-sandbox")))
 '(speedbar-default-position (quote left-right))
 '(speedbar-use-images nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;----------------------------------------------------------------------

(server-start)
