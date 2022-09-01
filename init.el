;; Note: do M-: user-init-file to find the loading path for the init.el file
(setq default-directory "C:/Users/Chiqui/") ;; Change default directory for emacs
(setq inhibit-startup-message t)
(setq use-short-answers t) ;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar
(setq visible-bell nil) ; Disable visible bell (flickering)
(setq make-backup-files nil) ; No backup files
(blink-cursor-mode 0) ; No blinking cursor
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; This makes ESC quit prompts (like ctrl+g)
(toggle-frame-fullscreen) ; Full screen emacs on startup
;; (global-hl-line-mode t) ; If you want to highlight the line of the cursor

;; Custom Keybindings
(global-set-key (kbd "M-q") 'kill-buffer-and-window)

; Fonts 
(set-frame-font "Consolas 9" nil t)
(set-frame-parameter (selected-frame) 'alpha '(99 . 90)) ; Transparency
(linum-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		writeroom-mode-hook))
  (add-hook mode (lambda ( ) (linum-mode 0))))

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

;; Use Ivy, Counsel, Whichkey and Helm for completions (better file navigation and suggested functions)
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

(use-package helm
    :config
    (require 'helm-config)
    :init
    (helm-mode 1)
    :bind
    (("M-x"     . helm-M-x) ;; Evaluate functions
     ("C-x C-f" . helm-find-files) ;; Open or create files
     ("C-x b"   . helm-mini) ;; Select buffers
     ("C-x C-r" . helm-recentf) ;; Select recently saved files
     ("C-c i"   . helm-imenu) ;; Select document heading
     ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
     :map helm-map
     ("C-z" . helm-select-action)
     ("<tab>" . helm-execute-persistent-action)))

(use-package general) ; customize key bindings
(general-define-key
  "C-M-j" 'counsel-switch-buffer) ; key shortcut for switching buffer

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)) ; this displays a description of the functions

(use-package ivy-posframe ;; see minibuffer in the center of the screen
  ;; :diminish
  :after ivy
  :custom
  (ivy-posframe-width 70)
  (ivy-posframe-height 15)
  (ivy-posframe-border-width 2)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package helm-posframe
  :config
  (helm-posframe-enable))
(setq helm-posframe-width 100)
		      
;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-height 15); to make it look better we need to download all the  icons (see below)
(use-package all-the-icons)
(line-number-mode 0) ;; No line number in the modeline
(column-number-mode 0) ;; No column number in the modeline

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
 :init (load-theme 'doom-one t)); I also like doom-old-hope doom-palenight 

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ; this allows to better distinguish parenthesis

; Bad boy mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

; Smooth scrolling
(setq scroll-margin 5 scroll-conservatively 0
scroll-up-aggressively 0.01
scroll-down-aggressively 0.01)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Writeroom (Zen mode/Goyo)
(use-package writeroom-mode
  :defer t
  :bind ("M-n" . writeroom-mode) ;; Toggle writeroom mode with Alt-n
  :config
  (setq writeroom-global-effects nil) ;; No need to have Writeroom do any of that silly stuff
  (setq writeroom-width 110))

;; Hide fringe curly arrows 
(set-fringe-mode 0)

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 5))) ;; number of recent files
    ;; (setq dashboard-show-shortcuts nil)
    ;; (setq dashboard-banner-logo-title "Hello")
    ;; (setq dashboard-center-content t)
    (setq dashbpard-set-file-icons t)
    (setq dashboard-set-heading-icons t))
    ;; (setq dashboard-startup-banner "tilde/Downloads/image.png")
    :config
    (dashboard-setup-startup-hook))
