(setq default-directory "C:/Users/Chiqui/")
(setq visible-bell 1)
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visible-bell t) ; Set up the visible bell
(setq make-backup-files nil) ; No backup files
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; This makes ESC quit prompts (like ctrl+g)
; Fonts 
(set-frame-font "Consolas 9" nil t)
(set-frame-parameter (selected-frame) 'alpha '(97 . 90)) ; Transparency
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda ( ) (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package) ; This brings into the environment all the package management functions

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) ; This initializes the package system and prepares it to be used
(unless package-archive-contents
  (package-refresh-contents)) ; Check if it already exists

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) ; install if it's not already installed (IMPORTANT: you need to install it in your system's font directory
  (package-install 'use-package))

(require 'use-package) ; once installed we load it using require
(setq use-package-always-ensure t) ; because it doesn't ensure packages by default

;; Use Ivy and Counsel for completions (better file navigation and suggested functions)
(use-package ivy
  :diminish
  :bind(("C-s" . swiper)
	:map ivy-minibuffer-map
	("TAB" . ivy-alt-done)
	("C-l" . ivy-alt-done)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-l" . ivy-done)
	("C-d" . ivy-switch-buffer-kill)
	:map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package general) ; customize key bindings
(general-define-key
  "C-M-j" 'counsel-switch-buffer) ; key shortcut for switching buffer

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-height 15); to make it look better we need to download all the  icons (see below)
(use-package all-the-icons)

;;; FANCY STUFF (nyan bar and highlight flash)
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package doom-themes
 :init (load-theme 'doom-old-hope t)); I also like doom-old-hope doom-palenight 

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ; this allows to better distinguish parenthesis

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)) ; this displays a description of the functions

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

; Bad boy mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

