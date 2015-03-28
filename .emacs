
;; Packages that I install:
;; helm, projectile, helm-projectile, sr-speedbar, projectile-speedbar
;; Note: installing helm-projectile will automatically install helm and
;; projectile, since they are dependencies. sr-speedbar and projectile-speedbar
;; will still need to be installed independently.

;; Miscellaneous settings

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (mouse-wheel-mode t)
  (when (or (not (boundp 'do-not-resize)) (not do-not-resize))
    (set-frame-size (selected-frame) 85 45)))
(show-paren-mode t)
(column-number-mode t)
(global-hi-lock-mode t)
(setq hl-line-color "#3E3D32")
; do not recenter on point when scrolling off screen
(setq scroll-conservatively 1)
; Tell Emacs to automatically place the point at the end of the compilation
; buffer.
(setq compilation-scroll-output t)
(setq org-replace-disputed-keys t)

;; Hook functions

; The next two functions are to be (sometimes) called by c++-hook and c-hook,
; depending on if we're developing on a machine that we want tabs instead of
; spaces.
(defun c++-tab-mode-helper ()
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(defun c-tab-mode-helper ()
  (setq c-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq tab-width 4)
  (setq indent-tabs-mode t))

(defun c++-hook ()
  ;(c++-tab-mode-helper) ; uncomment for tabs in C++.
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun c-hook ()
  ;(c-tab-mode-helper) ; uncomment for tabs in C.
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun emacs-lisp-hook ()
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun text-hook ()
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun sh-hook ()
  (setq indent-tabs-mode t)
  (setq tab-stop-list (number-sequence 8 200 8))
  (setq tab-width 8)
  (setq indent-line-function 'insert-tab)
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun python-hook ()
  (linum-mode t)
  (hl-line-mode t)
  (set-face-background 'hl-line hl-line-color))

(defun eshell-hook ()
  (setq pcomplete-cycle-completions nil))

(defun org-hook ()
  (org-indent-mode))

(add-hook 'c++-mode-hook 'c++-hook)
(add-hook 'c-mode-hook 'c-hook)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-hook)
(add-hook 'text-mode-hook 'text-hook)
(add-hook 'sh-mode-hook 'sh-hook)
(add-hook 'python-mode-hook 'python-hook)
(add-hook 'eshell-mode-hook 'eshell-hook)
(add-hook 'org-mode-hook 'org-hook)

;; Project management
;; Loading helm/projectile can take a second or two, and it isn't really needed
;; if we're just doing quick edits. Only load them when this function is
;; called.
(defun load-project-management ()
  (interactive)
  (require 'helm-config)
  (helm-mode 1)
  (projectile-global-mode)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (set-additional-project-keys))

;; Key binding functions

(defun small-scroll-down ()
  (interactive)
  (scroll-down 4))

(defun small-scroll-up ()
  (interactive)
  (scroll-up 4))

(defun move-backwards (count &optional all-frames)
  (interactive "p")
  (other-window (* -1 count) all-frames))

(defun reload-emacs-config ()
  (interactive)
  (setq do-not-resize t)
  (load-file "~/.emacs")
  (setq do-not-resize nil))

(defun highlight-all-current-region (&optional face)
  (interactive
   (list
    (hi-lock-read-face-name)))
  (or (facep face) (setq face 'hi-yellow))
  (hi-lock-set-pattern 
   (buffer-substring (mark) (point))
   face)
  (deactivate-mark))

(defun set-additional-project-keys ()
  (global-set-key (kbd "C-c h") 'projectile-grep))

(global-set-key [f1] 'server-start)
(global-set-key [f2] 'revert-buffer)
(global-set-key [f5] 'reload-emacs-config)
(global-set-key [f6] 'compile)
(global-set-key [f7] 'recompile)
(global-set-key [f8] 'load-project-management)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [home] 'small-scroll-down)
(global-set-key [end] 'small-scroll-up)
(global-set-key (kbd "C-<") 'small-scroll-down)
(global-set-key (kbd "C->") 'small-scroll-up)
(global-set-key (kbd "C-\"") 'other-window)
(global-set-key (kbd "C-:") 'move-backwards)
(global-set-key (kbd "C-x p") 'move-backwards)
(global-set-key [(control shift delete)] 'delete-region)
(global-set-key (kbd "C-x r") 'rename-buffer)
(global-set-key (kbd "C-x ,") 'kill-matching-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-a") 'back-to-indentation)
(global-set-key (kbd "C-c C-h") 'highlight-all-current-region)
(global-set-key (kbd "C-{") 'previous-buffer)
(global-set-key (kbd "C-}") 'next-buffer)

;; Style settings

(setq c-default-style "linux" c-basic-offset 3)
(setq c++-default-style "linux" c++-basic-offset 3)
(global-visual-line-mode t)
(when (display-graphic-p)
  (progn
    (set-scroll-bar-mode 'right)
    ; Colors from Monokai theme.
    (add-to-list 'default-frame-alist '(foreground-color . "#F8F8F2"))
    (add-to-list 'default-frame-alist '(background-color . "#272822"))
    (add-to-list 'default-frame-alist '(cursor-color . "#FFFFFF"))))
(set-face-attribute 'default nil :height 100)

; eshell custom prompt
(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize
	  (concat
	   "\n["
	   (format-time-string "%a %Y-%m-%d %H:%M:%S")
	   "]\n"
	   (car (reverse (split-string (eshell/pwd) "/")))
	   (if (= (user-uid) 0) " #" " $"))
	  'face `(:foreground "#00CC00"))
	 (propertize " " 'face `(:foreground "#FFFFFF")))))
(setq eshell-highlight-prompt nil)

;; Backup file behavior

(setq vc-make-backup-files t)
(setq version-control t
      kept-new-versions 4
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

; Courtesy of www.emacswiki.org/emacs/ForceBackups
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
	  (kept-new-versions 4))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;; Package management. This only works with emacs 24 or greater.

;; The list of packages to install when calling install-selected-packages.
(setq packages-to-install
      '(helm-projectile
	sr-speedbar
	projectile-speedbar))

;; General function for ensuring that a list of packages if installed.
(defun install-packages-if-not-installed (package-list)
  (if package-list
      (let ((curr-package (car package-list)))
	(progn
	  (if (not (package-installed-p curr-package))
	      (package-install curr-package))
	  (install-packages-if-not-installed (cdr package-list))))))

;; Interactive function to ensure that all packages in the list
;; "packages-to-install" are installed.
(defun install-selected-packages ()
  (interactive)
  (install-packages-if-not-installed packages-to-install))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t))

