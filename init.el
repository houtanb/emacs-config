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
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-style -1)

;; disable alarm bell beep
(setq visible-bell t)

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
(mouse-wheel-mode t)

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
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
    (local-set-key "\C-m" x)))
(add-hook 'octave-mode-hook 'RET-behaves-as-LFD)

(setq octave-auto-indent t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode (for jit colors)
;;
(cond (emacs-20-p
       ;; do lazy locking, it's quicker
       (setq font-lock-support-mode 'lazy-lock-mode)
       ;; more font-locking, variables for `lazy-lock-mode'
       ;; wait 10 secs before font-locking stuff
       (setq lazy-lock-defer-time 10
	     ;; don't font lock as I type
	     lazy-lock-defer-on-the-fly t
	     ;; If I'm not doing stuff, start fontifying
            ;; the rest of the buffer
	     lazy-lock-stealth-time 30))
      (emacs-21-p
       ;; emacs 21 has jit-lock which is better
       (setq font-lock-support-mode 'jit-lock-mode)
       (setq jit-lock-stealth-time 16
	     jit-lock-defer-contextually t
	     jit-lock-stealth-nice 0.5)
       (setq-default font-lock-multiline t)))
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
