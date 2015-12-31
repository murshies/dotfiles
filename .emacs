
;; Miscellaneous settings

(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(show-paren-mode t)
(column-number-mode t)
(global-hi-lock-mode t)
; Scroll when we're 2 row away from the edge of the window.
(setq smooth-scroll-margin 2)
; Tell Emacs to automatically place the point at the end of the compilation
; buffer.
(setq compilation-scroll-output t)
(setq org-replace-disputed-keys t)
(setq frame-title-format
      '(:eval (if buffer-file-name "%b (%f)" "%b")))
; New buffers have org-mode as the default
(setq-default major-mode 'text-mode)
(setq org-todo-keyword-faces '(("TODO" . hi-yellow)))
(setq-default indent-tabs-mode nil)
; The default blue is incredibly difficult to read
(set-face-attribute 'term-color-blue nil :foreground "SkyBlue")

(defun ssh-ansi (ssh-args)
  "Open an ssh connection in a new buffer.

This offers functionality similar to the ssh package, but uses an ansi-term
terminal instead of a shell-mode terminal. This function takes the ssh
arguments as one interactive parameter. The name of the new ssh buffer is based
off of the first ssh argument (space delimited).

This function assumes that there is an ssh command on the user's PATH."
  (interactive "sEnter ssh arguments: ")
  (let* ((buffer-name (concat "ssh " (car (split-string ssh-args))))
         (full-buffer-name
          (generate-new-buffer-name (concat "*" buffer-name "*")))
         (ssh-command (concat "ssh " ssh-args)))takes one interactive argument, which is
    (ansi-term "/bin/bash" buffer-name)
    (switch-to-buffer full-buffer-name)
    (insert ssh-command)
    (term-send-input)))

(defun highlight-line-mode ()
  (if (display-graphic-p)
      (progn
	(hl-line-mode t)
	(set-face-background 'hl-line hl-line-color))))

;; Frame hook setup
;; This defines a hook that will be run whenever a frame is created, or when
;; emacs is not started as a daemon.
(defun frame-creation-hook (frame)
  (with-selected-frame frame
    (when (not (eq (framep frame) t))
      (tool-bar-mode -1)
      (mouse-wheel-mode t)
      (setq hl-line-color "#3E3D32")
      (set-scroll-bar-mode 'right)
      ; Colors from Monokai theme.
      (set-foreground-color "#F8F8F2")
      (set-background-color "#272822")
      (set-cursor-color "#FFFFFF")
      (condition-case ex
          (set-frame-font "DejaVu Sans Mono")
        ('error t)))))

;; We need to do this check + call, since apparently starting emacs in
;; non-daemon mode doesn't count as creating a frame.
(when (not (daemonp)) (frame-creation-hook (selected-frame)))
(add-hook 'after-make-frame-functions 'frame-creation-hook)

;; Hook functions

(defun before-save ()
  "A hook that runs before a buffer is saved.
Currently this just deletes trailing whitespace if the buffer is using
python-mode."
  (let ((delete-whitespace-major-modes '(python-mode)))
    (when (member major-mode delete-whitespace-major-modes)
      (delete-trailing-whitespace))))
      

(defun c++-hook ()
  (linum-mode t)
  (highlight-line-mode)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (c-set-offset 'innamespace 0)
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
  (if (package-installed-p 'ggtags)
      (ggtags-mode)))

(defun c-hook ()
  (linum-mode t)
  (highlight-line-mode)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (if (package-installed-p 'ggtags)
      (ggtags-mode)))

(defun emacs-lisp-hook ()
  (linum-mode t)
  (highlight-line-mode))

(defun text-hook ()
  (linum-mode t)
  (highlight-line-mode))

(defun sh-hook ()
  (linum-mode t)
  (highlight-line-mode))

(defun python-hook ()
  (linum-mode t)
  (highlight-line-mode))

(defun eshell-hook ()
  (setq pcomplete-cycle-completions nil))

(defun org-hook ()
  (org-indent-mode)
  (setq org-log-done "time"))

(defun lisp-hook ()
  (highlight-line-mode)
  (linum-mode t))

(defun racket-hook ()
  (highlight-line-mode)
  (linum-mode t))

(defun lua-hook ()
  (highlight-line-mode)
  (linum-mode t)
  (setq lua-indent-level 4))

(defun web-hook ()
  (highlight-line-mode)
  (linum-mode t))

(defun yaml-hook ()
  (highlight-line-mode)
  (linum-mode t))

(add-hook 'c++-mode-hook 'c++-hook)
(add-hook 'c-mode-hook 'c-hook)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-hook)
(add-hook 'text-mode-hook 'text-hook)
(add-hook 'sh-mode-hook 'sh-hook)
(add-hook 'python-mode-hook 'python-hook)
(add-hook 'eshell-mode-hook 'eshell-hook)
(add-hook 'org-mode-hook 'org-hook)
(add-hook 'lisp-mode-hook 'lisp-hook)
(add-hook 'racket-mode-hook 'racket-hook)
(add-hook 'lua-mode-hook 'lua-hook)
(add-hook 'web-mode-hook 'web-hook)
(add-hook 'yaml-mode-hook 'yaml-hook)
(add-hook 'before-save-hook 'before-save)

;; Set up web-mode
;; Most of the following code was taken from:
;; http://emacs.stackexchange.com/questions/17010/in-web-mode-php-block-in-html-file-doesnt-syntax-highlight
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . web-mode))
(setq web-mode-engines-alist
      '(("php"   . "\\.html\\'")
        ("php"   . "\\.phtml\\'")
        ("blade" . "\\.blade\\.")))

;; Project management
;; Loading helm/projectile can take a second or two, and it isn't really needed
;; if we're just doing quick edits. Only load them when this function is
;; called.
(defun load-project-management ()
  (interactive)
  (require 'helm-config)
  (helm-mode 1)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-mode 'alien)
  (setq projectile-enable-caching t)
  (setq project-enable-caching t)
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

(defun prev-window (count &optional all-frames)
  (interactive "p")
  (other-window (- count) all-frames))

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

(defun determine-projectile-search-program ()
  (cond
   ((executable-find "ag") 'helm-projectile-ag)
   ((executable-find "ack") 'helm-projectile-ack)
   (t 'helm-projectile-grep)))

(defun set-additional-project-keys ()
  (global-set-key (kbd "C-c h") (determine-projectile-search-program))
  (global-set-key (kbd "C-c p w") 'projectile-global-mode)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-select-action)
  (define-key helm-map (kbd "<backtab>") 'helm-find-files-up-one-level))

(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-v") 'pyflakes-current-file))

(defun pyflakes-current-file ()
  "Run pyflakes on the current file.
Even though the documentation for python-check says that the default file is
supposed to be the current buffer's default, this isn't true after the first
time that pyflakes it run. This function fixes this issue."
  (interactive)
  (python-check (concat "pyflakes " buffer-file-name)))
  

(defun matches-any-regex (regex-list str)
  (if (not regex-list) nil
    (let ((next-regex (car regex-list)))
      (if (string-match-p next-regex str) t
	(matches-any-regex (cdr regex-list) str)))))

(setq buffer-regexs-to-hide
      '("*grep*" "*Help*" "*Messages*" "^*Python check" "*Backtrace*"
	"*Shell Command Output*" "*Process List*"))

(defun delete-windows-with-names (open-windows buffer-names)
  (if open-windows
      (let ((curr-window (car open-windows)))
	(progn
	  (if (matches-any-regex
	       buffer-names
	       (buffer-name (window-buffer curr-window)))
	      (delete-window curr-window))
	  (delete-windows-with-names (cdr open-windows) buffer-names)))))

(defun delete-specific-windows ()
  (interactive)
  (delete-windows-with-names (window-list) buffer-regexs-to-hide))

(defun create-new-buffer ()
  (interactive)
  (let ((new-buf (generate-new-buffer "new")))
    (switch-to-buffer new-buf)
    (set-buffer-major-mode new-buf)))

(defun tramp-cleanup-all ()
  "Clean up all tramp connections/buffers. If the current directory of the
eshell buffer is remote, change the directory to the user's home directory
before doing the cleanup. This prevents tramp-cleanup-all-buffers from deleting
the eshell buffer as part of its cleanup."
  (interactive)
  (with-current-buffer "*eshell*"
    (when (string-match-p "ssh:" default-directory)
      (eshell/cd "~")
      (eshell-interrupt-process))
    (tramp-cleanup-all-buffers)))

(defun window-browser ()
  "Enter an interactive browsing mode, where the following keys are mapped to
specific window navigation functions:
 [ (91) - display the 'previous buffer' in the current window
 ] (93) - display the 'next buffer' in the current window
 ; (59) - move focus to the previous window
 ' (39) - move focus to the next window
 , (44) - scroll down in the current buffer (emacs' definition of scroll down)
 . (46) - scroll up in the current buffer
 0 (48) - delete current window
 1 (49) - delete all other windows
 2 (50) - split window below
 3 (51) - split window right
 < (60) - beginning of buffer
 > (62) - end of buffer
Entering any other key or key chord exits the browsing mode."
  (interactive)
  (let ((input-done nil))
    (while (not input-done)
      ; Catch any errors. Whenever the user tries to scroll off the edges of a
      ; buffer, Emacs treats this as an error and will exit window browsing.
      ; This behavior is undesired; instead, the window browsing session should
      ; continue.
      (condition-case ex
	  (let ((char-input (read-char "Browsing")))
	    (cond
	     ((= char-input 91) (previous-buffer))
	     ((= char-input 93) (next-buffer))
	     ((= char-input 59) (prev-window 1))
	     ((= char-input 39) (other-window 1))
	     ((= char-input 44) (small-scroll-down))
	     ((= char-input 46) (small-scroll-up))
	     ((= char-input 48) (delete-window))
	     ((= char-input 49) (delete-other-windows))
	     ((= char-input 50) (split-window-below))
	     ((= char-input 51) (split-window-right))
	     ((= char-input 60) (beginning-of-buffer))
	     ((= char-input 62) (end-of-buffer))
	     (t (setq input-done t))))
	('error t)))))
  
(global-set-key [f1] 'server-start)
(global-set-key [f2] 'revert-buffer)
(global-set-key [f5] 'reload-emacs-config)
(global-set-key [f6] 'compile)
(global-set-key [f7] 'recompile)
(global-set-key [f8] 'load-project-management)
(global-set-key [f9] 'delete-specific-windows)
(global-set-key [f12] 'tramp-cleanup-all)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key [(meta left)] 'backward-sexp)
(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [(control shift delete)] 'delete-region)
(global-set-key (kbd "C-x r") 'rename-buffer)
(global-set-key (kbd "C-x ,") 'kill-matching-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-a") 'back-to-indentation)
(global-set-key (kbd "C-c C-h") 'highlight-all-current-region)
(global-set-key (kbd "C-S-n") 'create-new-buffer)
(global-set-key (kbd "M-P") 'window-browser)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c d") 'eshell-cd-to-current-directory)
(global-set-key (kbd "M-I") 'windmove-up)
(global-set-key (kbd "M-K") 'windmove-down)
(global-set-key (kbd "M-J") 'windmove-left)
(global-set-key (kbd "M-L") 'windmove-right)
(global-set-key (kbd "C-x j") 'join-line)
(global-set-key (kbd "C-{") 'previous-buffer)
(global-set-key (kbd "C-}") 'next-buffer)
(global-set-key (kbd "C-<") 'small-scroll-down)
(global-set-key (kbd "C->") 'small-scroll-up)
(global-set-key (kbd "C-\"") 'other-window)
(global-set-key (kbd "C-:") 'move-backwards)
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Style settings

(global-visual-line-mode t)
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

(defun eshell-cd-to-current-directory ()
  (interactive)
  (let ((current-directory default-directory))
    (eshell)
    (eshell/cd current-directory)
    (eshell-interrupt-process)))

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
      '(erc-hl-nicks
	helm-ag
        helm-projectile
	redo+
	smooth-scrolling
        ssh
        yaml-mode))

;; General function for ensuring that a list of packages is installed.
	
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
  (package-refresh-contents)
  (install-packages-if-not-installed packages-to-install))

(defun load-redo+ ()
  (if (package-installed-p 'redo+)
      (progn
	(require 'redo+)
	(global-set-key (kbd "M-_") 'redo))))

(cond
 ((>= emacs-major-version 24)
  (progn
    (require 'package)
    (package-initialize)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (load-redo+)
    (add-to-list 'display-buffer-alist
                 '("." nil (reusable-frames . t)))))
 (t
  (progn
    (setq-default display-buffer-reuse-frames t))))
