;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load path
;;
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default theme
;;
(load-theme 'misterioso)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;
;; no gui junk
; useful for Emacs GUI downloaded from site, not for homebrew
;(tool-bar-mode -1)
;(menu-bar-mode -1)
;(tooltip-mode -1)
;(set-fringe-style -1)

;; disable alarm bell beep
(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell       nil
      ring-bell-function #'my-terminal-visible-bell)

;; disable startup screen
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; use Monaco font in Mac OS X
(when (eq system-type 'darwin)
  (set-default-font "Monaco"))

;; highlight current line
;(global-hl-line-mode 1)

; Set cursor color to white
(set-cursor-color "#ffffff")

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; indent 2 spaces
(setq standard-indent 2)

;; line-by-line scrolling
(setq scroll-step 1)

;; tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; scroll with mouse wheel
;(mouse-wheel-mode t)

;; column and line numbering
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode t)

;; wrap at 79 chars
(setq-default fill-column 79)

;; text mode default for new buffer
(setq-default major-mode 'text-mode)

;; support accents on chars
(set-language-environment "Latin-1")

;; enable copy/paste with clipboard
(setq x-select-enable-clipboard t)
(setq locale-preferred-coding-systems '((".*" . utf-8)))

;; switch between frames
;(global-set-key (kbd "C-x <up>") 'windmove-up)
;(global-set-key (kbd "C-x <down>") 'windmove-down)
;(global-set-key (kbd "C-x <left>") 'windmove-left)
;(global-set-key (kbd "C-x <right>") 'windmove-right)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight matching parens
(show-paren-mode t)

;; enable interactive do
(ido-mode t)

;; ido flex match
(add-hook 'ido-setup-hook (lambda ()
                            (setq ido-enable-flex-matching t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRAMP setup
;;
(require 'tramp nil t)
(setq tramp-default-method "ssh")
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Octave mode
;;
;; autolad octave mode for *.m-files
;(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
;(add-hook 'octave-mode-hook
;          (lambda ()
;            (abbrev-mode 1)
;            (auto-fill-mode 1)
;            (font-lock-mode 1)))

;(setq octave-auto-indent t)
;(setq octave-block-offset 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab mode
;(load-library "matlab-load")
;; Enable CEDET feature support for MATLAB code. (Optional)
;    (matlab-cedet-setup)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode (for jit colors)
;;
;; emacs 21 has jit-lock which is better
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bison
;;
(autoload 'bison-mode "bison-mode.el")
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . bison-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flex
;;
(autoload 'flex-mode "flex-mode")
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . flex-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia
;;
;(autoload 'julia-mode "julia-mode")
;(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'julia-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; use mouse in xterm
;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
