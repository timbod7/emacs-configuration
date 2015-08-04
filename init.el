;; Require Emacs' package functionality

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa-stable.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

; The following packages need to be installed
;     haskell-mode
;     ghc
;     org-present
;     projectile
;     flx-ido
;     magit
;     ido-vertical-mode
;     markdown-mode
;     neotree


;----------------------------------------------------------------------
; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)
 
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
; Haskell Mode

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;----------------------------------------------------------------------
; Purescript mode
;
; Installed with:
;   cd ~/.emacs.d
;   git clone https://github.com/dysinger/purescript-mode.git
;   make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs all

(add-to-list 'load-path "~/.emacs.d/purescript-mode/")
(require 'purescript-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/lib/emacs/purescript-mode/")

;----------------------------------------------------------------------
; ghc-mod support
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defvar ghc-ghc-options '("-fno-warn-name-shadowing") "*GHC options")
(add-hook 'haskell-mode-hook
          (lambda () (ghc-init) )
          )
                  

;----------------------------------------------------------------------
; Org present mode

(require 'org-present)

;----------------------------------------------------------------------
; key-bindings

(global-unset-key "\C-x@")
(global-set-key "\C-x@" 'compile)
(global-set-key "\C-xg" `magit-status)

; control-c c ... is my personal keymap

(global-set-key "\C-ccc" 'comment-region)
(global-set-key "\C-cct" 'neotree-toggle)


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
; Unfill a paragraph

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

;-----------------------------------------------------------------------
; File backups and autosaves to temp directory
(defvar autosave-dir "~/tmp/emacs_autosaves/")
(make-directory autosave-dir t)

(setq backup-directory-alist
          `((".*" . ,autosave-dir)))
(setq auto-save-file-name-transforms
          `((".*" ,autosave-dir t)))

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
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;----------------------------------------------------------------------
; neotree configuration
(require 'neotree)

; This is not yet in the released neotree distribution, so paste it here

(defun neotree-projectile-action ()
  "Integration with `Projectile'.
Usage:
    (setq projectile-switch-project-action 'neotree-projectile-action).
When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically."
  (interactive)
  (cond
   ((fboundp 'projectile-project-root)
    (neotree-dir (projectile-project-root)))
   (t
    (error "Projectile is not available"))))

;(setq projectile-switch-project-action 'neotree-projectile-action)

;----------------------------------------------------------------------
; Customisation

(set-background-color "white")
(set-foreground-color "black")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "twd")
     (c++-mode . "twd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(exec-path
   (quote
    ("/Users/timd/bin" "/Applications/ghc-7.8.4.app/Contents/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(font-use-system-font t)
 '(js-indent-level 2)
 '(neo-show-header nil)
 '(neo-window-width 32)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" ".cabal-sandbox")))
 '(projectile-use-git-grep t)
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
