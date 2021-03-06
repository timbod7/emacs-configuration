;; Require Emacs' package functionality

(require 'package)

;----------------------------------------------------------------------
; see http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

; list the packages we need
(setq package-list
  '(haskell-mode
    ghc
;    org-present
    projectile
    flx-ido
    ido-vertical-mode
    markdown-mode
    neotree
    js2-mode
    json-mode
    helm
;    color-theme-sanityinc-tomorrow
    intero
    typescript-mode
    tide
    ))

; list the repositories containing them
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("melpa" . "http://melpa-stable.milkbox.net/packages/")
    ))


; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available if we don't
; have it already
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; install any missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;----------------------------------------------------------------------
; Confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;----------------------------------------------------------------------
; Never split windows horizontally
(setq split-width-threshold 9999)

;----------------------------------------------------------------------
; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;----------------------------------------------------------------------
; Helm mode
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action)              ; list actions using C-z

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
; Scaling

(define-key global-map (kbd "s-=") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)

;----------------------------------------------------------------------
; Haskell Mode

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
; (add-hook 'haskell-mode-hook 'intero-mode)

;----------------------------------------------------------------------
; Javascript

(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)

;----------------------------------------------------------------------
; Typescript
; see https://github.com/ananthakumaran/tide

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun tide-mode-setup ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;----------------------------------------------------------------------
; Markdown mode
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;----------------------------------------------------------------------
; Purescript mode
;
; Installed with:
;   cd ~/.emacs.d
;   git clone https://github.com/dysinger/purescript-mode.git
;   make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs all

;(add-to-list 'load-path "~/.emacs.d/purescript-mode/")
;(require 'purescript-mode-autoloads)
;(add-to-list 'Info-default-directory-list "~/.emacs.d/purescript-mode/")
;(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

;----------------------------------------------------------------------
; ghc-mod support
;(autoload 'ghc-init "ghc" nil t)
; (autoload 'ghc-debug "ghc" nil t)

; (defvar ghc-ghc-options '("-fno-warn-name-shadowing") "*GHC options")
;(add-hook 'haskell-mode-hook
;          (lambda () (ghc-init) )
;          )


;----------------------------------------------------------------------
; Org present mode

;(require 'org-present)

;----------------------------------------------------------------------
; personal key-bindings

(global-unset-key "\C-x@")
(global-set-key "\C-x@" 'compile)
(global-set-key "\C-xg" `magit-status)

(global-unset-key "\C-z")     ; Minimising on OSX crashes :-( so disable it
(global-unset-key "\C-xz")
(global-unset-key "\C-x\C-z")

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-show)
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(global-set-key "\C-ccc" 'comment-region)
(global-set-key "\C-ctt" 'neotree-toggle)
(global-set-key "\C-ctp" 'neotree-project-dir)
(global-set-key "\C-ctf" 'neotree-find)


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
; java formatting

(add-hook 'java-mode-hook (lambda ()
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro 0)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-close '-)
  ))

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

; Disable bold text
(set-face-bold-p 'bold nil)

;----------------------------------------------------------------------
; projectile configuration
;
(require 'projectile)
(projectile-global-mode)
;(require 'flx-ido)
;(flx-ido-mode 1)
;(require 'ido-vertical-mode)
;(ido-vertical-mode 1)
(setq projectile-completion-system 'helm)
;(helm-projectile-on)

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
    ("/home/timd/.nix-profile/bin" "/home/timd/.nix-profile/sbin" "/home/timd/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/lib/emacs/24.5/x86_64-linux-gnu")))
 '(fringe-mode 0 nil (fringe))
 '(js-indent-level 2)
 '(js2-include-node-externs t)
 '(menu-bar-mode nil)
 '(neo-show-header nil)
 '(neo-theme (quote arrow))
 '(neo-window-width 32)
 '(package-selected-packages
   (quote
    (tide ## purescript-mode typescript-mode yaml-mode org-present neotree markdown-mode magit js2-mode intero ido-vertical-mode helm-projectile ghc flx-ido exec-path-from-shell color-theme-sanityinc-tomorrow)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" ".cabal-sandbox")))
 '(projectile-use-git-grep t)
 '(speedbar-default-position (quote left-right))
 '(speedbar-use-images nil)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

;----------------------------------------------------------------------

(server-start)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(fixed-pitch ((t (:inherit default)))))
