;; ============================================================================
;; Miscellaneous settings
;; ============================================================================

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
(setq buffer-regexs-to-hide
      '("*grep*" "*Help*" "*Messages*" "^*Python check" "*Backtrace*"
        "*Shell Command Output*" "*Process List*"))
(global-visual-line-mode t)
(set-face-attribute 'default nil :height 100)
(setq tags-add-tables nil)

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

;; (setq-default left-fringe-width 10)
;; (setq-default right-fringe-width 0)
;; (set-face-attribute 'fringe nil :background "black")

;; ============================================================================
;; Utility functions
;; ============================================================================

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
         (ssh-command (concat "ssh " ssh-args)))
    (ansi-term "/bin/bash" buffer-name)
    (switch-to-buffer full-buffer-name)
    (insert ssh-command)
    (term-send-input)))

(defun highlight-line-mode ()
  (if (display-graphic-p)
      (progn
        (hl-line-mode t)
        (set-face-background 'hl-line hl-line-color))))

(defun copy-line ()
  "Copies the entire current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-end-position)))

(defun try-to-load-elisp (elisp-file)
  "Try to call load-file on elisp-file.
If the file cannot be loaded, display an error message."
  (condition-case ex
      (load-file elisp-file)
    ('error (message "%s" ex))))

(defun load-lisp-in-dir (dir)
  "Load all elisp files (ending in .el extension) in dir.
If there is an issue reading dir, display an error message."
  (if (file-exists-p dir)
      (condition-case ex
          (mapc 'try-to-load-elisp (directory-files dir t "\\.el$"))
        ('error (message "%s" ex)))))

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
  "Reload ~/.emacs
When a prefix argument is used, do not reload the files in ~/elisp"
  (interactive)
  (when current-prefix-arg (setq reload-elisp nil))
  (load-file "~/.emacs")
  (setq reload-elisp t))

(defun highlight-all-current-region (&optional face)
  (interactive
   (list
    (hi-lock-read-face-name)))
  (or (facep face) (setq face 'hi-yellow))
  (hi-lock-set-pattern 
   (buffer-substring (mark) (point))
   face)
  (deactivate-mark))

(defun matches-any-regex (regex-list str)
  (if (not regex-list) nil
    (let ((next-regex (car regex-list)))
      (if (string-match-p next-regex str) t
        (matches-any-regex (cdr regex-list) str)))))

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
  (when (get-buffer "*eshell*")
    (with-current-buffer "*eshell*"
      (when (string-match-p "ssh:" default-directory)
        (eshell/cd "~")
        (eshell-interrupt-process))))
  (tramp-cleanup-all-buffers))

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

(defun search-all-buffers (regex)
  "Search all open buffers for lines matching regex."
  (interactive "sList lines match regex: ")
  (multi-occur-in-matching-buffers ".*" regex))

(defun eshell-cd-to-current-directory ()
  (interactive)
  (let ((current-directory default-directory))
    (eshell)
    (eshell/cd current-directory)
    (eshell-interrupt-process)))

(defun term-cd-to-starting-directory ()
  "Change the current directory in the current term-mode buffer to its starting
buffer. This relies on the current term-mode buffer having a variable defined
called starting-directory."
  (interactive)
  (interrupt-process)
  (insert (concat "cd " starting-directory))
  (term-send-input))

(defun named-ansi-term (program)
  "Creates an ansi-term buffer with a customized name. The format of the name
is *ansi-<current directory>*."
  (interactive
   (list
    (read-string "Run program: " (or explicit-shell-file-name "/bin/bash") nil
                 (or explicit-shell-file-name "/bin/bash"))))
  (let* ((absolute-directory
          (directory-file-name (expand-file-name default-directory)))
         (cd-name (car (last (split-string absolute-directory "/"))))
         (buffer-name (concat "ansi-" cd-name)))
    (ansi-term program buffer-name)))

(defun ansi-term-in-directory (directory)
  "Creates an ansi term in the directory that is interactively entered.
named-ansi-term is called interactively to allow the user to enter the program
that will be run."
  (interactive (list (read-file-name "Enter the shell starting directory: ")))
  (let ((default-directory (file-name-as-directory directory)))
    (call-interactively 'named-ansi-term)))

(defun generate-etags-in-directory (directory file-patterns)
  "Generate an etags file in a directory specified by the user. The user also
specifies a pattern (passed to find) that will match against files in the
specified directory and subdirectories."
  (interactive
   (list
    (read-file-name "Enter the etags root: ")
    (read-regexp "Enter any number of file name patterns, separated by spaces: ")))
  (let* ((tag-file (concat (file-name-as-directory directory) "TAGS"))
         (file-pattern-list
          (cl-remove-if (lambda (s) (= (length s) 0))
                        (split-string file-patterns)))
         (find-file-string
          (mapconcat 'identity file-pattern-list "\" -or -name \""))
         (etags-command
          (format "find \"%s\" -type f -name \"%s\" | etags -f %s -"
                  directory find-file-string tag-file)))
    (compilation-start etags-command nil
                       (lambda (_) (format "*etags [%s %s]*"
                                           directory file-patterns)))))

;; ============================================================================
;; Hooks and mode-specific setup
;; ============================================================================

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

(defun before-save ()
  "A hook that runs before a buffer is saved.
Currently this just deletes trailing whitespace if the buffer is using
python-mode."
  (let ((delete-whitespace-major-modes
         '(python-mode
           c++-mode
           c-mode)))
    (when (member major-mode delete-whitespace-major-modes)
      (delete-trailing-whitespace))))

(defun linum-and-hl-line-hook ()
  "Defines a base mode hook that enables linum and hl-line minor modes.
There are many modes on which these should be enabled, so instead of defining a
function for each and then adding the mode hook separately, this function can
be applied to each major mode in a smarter way."
  (linum-mode)
  (highlight-line-mode))

;; (setq linum-format
;;       (lambda (num) (if (display-graphic-p) (format "%d" num)
;;                       (format "%d " num))))

(setq modes-for-linum-and-hl-line
      '(c++-mode-hook
        c-mode-hook
        conf-mode-hook
        emacs-lisp-mode-hook
        js-mode-hook
        lua-mode-hook
        lisp-mode-hook
        makefile-mode-hook
        org-mode-hook
        python-mode-hook
        racket-mode-hook
        sh-mode-hook
        text-mode-hook
        web-mode-hook
        yaml-mode-hook))

(mapc (lambda (mode-name) (add-hook mode-name 'linum-and-hl-line-hook))
      modes-for-linum-and-hl-line)

(defun c-common-hook ()
  "A hook used for C/C++."
  (setq c-default--style "linux")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace 0)) ; Don't indent for namespaces

(defun c++-hook ()
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file))

(defun c-hook ()
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file))

(defun eshell-hook ()
  (setq pcomplete-cycle-completions nil))

(defun org-hook ()
  (org-indent-mode)
  (setq org-log-done "time")
  ; two spaces in addition to the default two
  (setq org-list-indent-offset 2))

(defun lua-hook ()
  (setq lua-indent-level 4))

(defun term-hook ()
  ; The default blue is incredibly difficult to read
  (set-face-attribute 'term-color-blue nil :foreground "SkyBlue")
  (set-face-attribute 'term-color-red nil :foreground "Orchid")
  (make-local-variable 'starting-directory)
  (setq starting-directory default-directory)
  (define-key term-mode-map (kbd "C-c g") 'term-cd-to-starting-directory)
  (define-key term-raw-map (kbd "C-c g") 'term-cd-to-starting-directory)
  (define-key term-raw-map (kbd "C-c s") nil)
  (define-key term-raw-map (kbd "M-x") 'helm-M-x)
  (define-key term-raw-map (kbd "M-:") 'eval-expression))

(add-hook 'c-mode-common-hook 'c-common-hook)
(add-hook 'c++-mode-hook 'c++-hook)
(add-hook 'c-mode-hook 'c-hook)
(add-hook 'eshell-mode-hook 'eshell-hook)
(add-hook 'org-mode-hook 'org-hook)
(add-hook 'lua-mode-hook 'lua-hook)
(add-hook 'term-mode-hook 'term-hook)
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

;; Use conf-mode for rc files
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode))

;; Define key bindings for python-mode when it is loaded. This is usually done
;; when the first python file of the emacs session is opened.
(eval-after-load "python"
  '(progn (define-key python-mode-map (kbd "C-c v f") 'pyflakes-current-file)
          (define-key python-mode-map (kbd "C-c v l") 'pylint-current-file)))

(defun pyflakes-current-file ()
  "Run pyflakes on the current file.
Even though the documentation for python-check says that the default file is
supposed to be the current buffer's default, this isn't true after the first
time that pyflakes it run. This function fixes this issue."
  (interactive)
  (python-check (concat "pyflakes " buffer-file-name)))

(defun pylint-current-file ()
  "Run pylint on the current file.
This runs in the same way as pyflakes-current-file (inside a compilation-mode
buffer), but with pylint instead. It will use the default .pylintrc file."
  (interactive)
  (python-check (concat "pylint -f text " buffer-file-name)))  

;; ============================================================================
;; Project management
;; ============================================================================

;; Loading helm/projectile can take a second or two, and it isn't really needed
;; if we're just doing quick edits. Only load them when this function is
;; called.
(defun load-project-management ()
  (interactive)
  (require 'helm-config)
  (helm-mode 1)
  ;; Turn on projectile-mode briefly to have emacs do project cache
  ;; initialization. Then, turn projectile mode off.
  (projectile-mode)
  (projectile-mode -1)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-mode 'alien)
  (setq projectile-enable-caching t)
  (setq project-enable-caching t)
  (set-face-foreground 'helm-etags-file "SkyBlue")
  (set-additional-project-keys))

(defun determine-projectile-search-program ()
  (cond
   ((executable-find "ag") 'helm-projectile-ag)
   ((executable-find "ack") 'helm-projectile-ack)
   (t 'helm-projectile-grep)))

(defun set-additional-project-keys ()
  (global-set-key (kbd "C-c h") (determine-projectile-search-program))
  (global-set-key (kbd "C-c p w") 'projectile-global-mode)
  (global-set-key (kbd "M-.") 'helm-etags-select)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-select-action)
  (define-key helm-map (kbd "<backtab>") 'helm-find-files-up-one-level)
  ;; The rest of these are normally a part of projectile-global-mode.
  (global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  (global-set-key (kbd "C-c p i") 'projectile-invalidate-cache)
  (global-set-key (kbd "C-c p p") 'helm-projectile-switch-project))

;; ============================================================================
;; Global key bindings
;; ============================================================================

(global-set-key [f1] 'search-all-buffers)
(global-set-key [f2] 'revert-buffer)
(global-set-key [f5] 'reload-emacs-config)
(global-set-key [f6] 'compile)
(global-set-key [f7] 'recompile)
(global-set-key [f8] 'load-project-management)
(global-set-key [f9] 'delete-specific-windows)
(global-set-key [f12] 'tramp-cleanup-all)
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
(global-set-key (kbd "C-c s e") 'eshell)
(global-set-key (kbd "C-c s d") 'eshell-cd-to-current-directory)
(global-set-key (kbd "C-c s a") 'named-ansi-term)
(global-set-key (kbd "C-c s s") 'ssh-ansi)
(global-set-key (kbd "C-c s w") 'ansi-term-in-directory)
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
(global-set-key (kbd "M-W") 'copy-line)
(global-set-key (kbd "M-g v") 'magit-status)

;; ============================================================================
;; Backup file behavior
;; ============================================================================

(setq vc-make-backup-files t)
(setq version-control t
      kept-new-versions 4
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; ============================================================================
;; Package management
;; ============================================================================

;; The list of packages to install when calling install-selected-packages.
(setq packages-to-install
      '(erc-hl-nicks
        helm-ag
        helm-projectile
        magit
        markdown-mode
        redo+
        smooth-scrolling
        web-mode
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

(when (or (not (boundp 'reload-elisp)) reload-elisp)
  (load-lisp-in-dir "~/elisp"))
