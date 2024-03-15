;; ============================================================================
;; Miscellaneous settings
;; ============================================================================

(blink-cursor-mode -1)
(column-number-mode)
(delete-selection-mode)
(show-paren-mode)
(defconst default-font-size 110)
(defvar font-size default-font-size)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hi-lock-mode)
(global-visual-line-mode)
(setcdr visual-line-mode-map (cdr (make-sparse-keymap)))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(set-face-attribute 'default nil :height font-size)
;; Tell Emacs to automatically place the point at the end of the compilation
;; buffer.
(setq compilation-scroll-output t)
(setq create-lockfiles nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq erc-join-buffer 'bury)
(setq-default
 header-line-format
 '(:eval (let* ((full-header buffer-file-name)
                (full-header-length (length full-header))
                (window-width (window-body-width)))
           (if full-header
               (if (> full-header-length window-width)
                   (concat "..." (substring full-header (- full-header-length (- window-width 3))))
                 full-header)
             "%b"))))
(setq inhibit-startup-screen t)
(setq org-replace-disputed-keys t)
(setq org-todo-keyword-faces '(("TODO" . hi-yellow)))
(setq ring-bell-function 'ignore)
(setq smerge-command-prefix (kbd "C-c v"))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default major-mode 'text-mode)
(setq-default fill-column 79)
(when (require 'package nil 'noerror)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (setq custom-file "~/.emacs.d/custom.el"))
(when (daemonp)
  (setenv "EDITOR" "emacsclient")
  (setenv "EMACS_DAEMON" "yes"))
;; Note: loading features that were downloaded from the package manager must be
;; done after calling package-initialize.
(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))
(require 'dired-x nil 'noerror) ;; for dired-jump
(require 'subr-x)
(setq dabbrev-case-fold-search nil)
(setq mouse-wheel-scroll-amount '(4))
(setq epa-pinentry-mode 'loopback)
(with-eval-after-load "json"
  (setq json-encoding-default-indentation "    "))
(setq native-comp-async-report-warnings-errors 'silent)
(setq eldoc-documentation-strategy #'eldoc-documentation-compose)

(defun set-font-size (size)
  (interactive
   (list (read-number (format "Font size (currently %d): " font-size))))
  (setq font-size size)
  (set-face-attribute 'default nil :height font-size))

(set-font-size font-size)

(defun bump-font-size-up ()
  (interactive)
  (set-font-size (+ font-size 10))
  (message "font size %d" font-size))

(defun bump-font-size-down ()
  (interactive)
  (set-font-size (- font-size 10))
  (message "font size %d" font-size))

(defun bump-default-font-size ()
  (interactive)
  (set-font-size default-font-size)
  (message "font size %d" font-size))

(defun git-quick-status()
  (interactive)
  (shell-command "git rev-parse --abbrev-ref HEAD && git rev-parse HEAD"))

(defun set-title-prefix (title-prefix)
  (interactive "sEnter prefix: ")
  (setq frame-title-prefix title-prefix))

(defun get-ssh-config-hosts ()
  "Get a list of hosts from the ssh config file.
This function filters out all hosts with wildcards (*, ?, and !)."
  (let ((ssh-config-file (format "%s/.ssh/config" (getenv "HOME"))))
    (if (file-exists-p ssh-config-file)
        (let* ((ssh-config-contents (with-temp-buffer
                                      (insert-file-contents ssh-config-file)
                                      (split-string (buffer-string) "\n" t)))
               (hosts (delq nil (mapcar (lambda (s)
                                          (when (string-match "^Host \\([^*?!]+\\)$" s)
                                            (match-string 1 s)))
                                        ssh-config-contents))))
          hosts)
      '())))

(defun json-lint ()
  "Lint a section of text from a buffer to check if it is valid JSON.

The point should be positioned at the start of the JSON text to
valid. This is essentially an interactive wrapper around
`json-parse-buffer'."
  (interactive)
  (json-parse-buffer)
  (message "JSON is valid"))

;; eshell settings
(defun eshell/e (file &rest files)
  "Open a list of files.
This function is meant to be called as a command in eshell. Wildcards are
supported."
  (let* ((unflattened (append (list file) files))
         (flattened (eshell-flatten-list unflattened)))
    (mapc #'find-file flattened)))

(defun eshell/a (cmd &rest args)
  "Runs an asynchronous command.
This is the same as running run-async-shell-command with no prefix argument and
the default directory as the eshell buffer's default directory."
  (let ((full-cmd (string-join (cons cmd args) " ")))
    (run-background-command
     full-cmd
     default-directory
     (background-command-buffer full-cmd))))

(defun eshell/v (cmd &rest args)
  "Runs an asynchronous command in a vterm buffer."
  (let* ((full-cmd (string-join (cons cmd args) " "))
         (vterm-buffer-name (format "*vterm async %s" full-cmd)))
    (vterm t)
    (vterm-insert (format "%s\n" full-cmd))))

(defun eshell/ll (&rest args)
  "Shorthand for ls -l in eshell"
  (eshell/ls "-l" args))

(defun eshell/lah (&rest args)
  "Shorthand for ls -lah in eshell"
  (eshell/ls "-lah" args))

(defun eshell/cds (&optional dir)
  "Visit a directory as root.
The directory is optional. WHen it isn't provided, visit the
current directory as root."
  (let ((dir-to-visit (concat "/sudo::" (if dir dir default-directory))))
    (eshell/cd dir-to-visit)))

(defun eshell/cd-start-dir ()
  "Change to the starting directory of the eshell buffer"
  (eshell/cd eshell-starting-directory))

(defun eshell-in-dir (starting-dir)
  "Start a new eshell in a specific directory"
  (interactive "DEnter the starting directory: ")
  (let ((default-directory starting-dir)
        (eshell-buffer-name (format "*eshell %s*" starting-dir)))
    (eshell)
    (setq eshell-starting-directory starting-dir)
    (eshell/cd starting-dir)))

(defun insert-tramp-host ()
  "In remote buffers, insert the tramp host."
  (interactive)
  (insert (or (file-remote-p default-directory) "/")))

(defun eshell-hook ()
  (make-local-variable 'eshell-starting-directory)
  (setq eshell-starting-directory default-directory)
  (define-key eshell-mode-map (kbd "C-c r") 'insert-tramp-host))

(add-hook 'eshell-mode-hook 'eshell-hook)

;; eshell custom prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         "\n"
         (propertize (concat "[" (eshell/pwd) "]")
                     'face '(:foreground "#8AE234" :weight bold))
         "\n"
         (propertize (concat user-login-name "@" system-name)
                     'face '(:foreground "Sky Blue" :weight bold))
         (if (= (user-uid) 0) " # " " $ "))))

(setq eshell-scroll-to-bottom-on-input t)

(defun essh (ssh-params)
  (interactive
   (list
    (completing-read "Enter SSH host: " (get-ssh-config-hosts))))
  (let ((default-directory (format "/ssh:%s:~" ssh-params))
        (eshell-buffer-name (format "*eshell ssh %s*" ssh-params)))
    (eshell t)))

(setq vc-handled-backends nil)

;; ============================================================================
;; Utility functions
;; ============================================================================

(defun highlight-line-mode ()
  (if (display-graphic-p)
      (progn
        (hl-line-mode t)
        (set-face-background 'hl-line hl-line-color))))

(defun copy-line-at-indentation ()
  "Copies the current line to the kill ring, excluding indentation."
  (interactive)
  (kill-new (string-trim (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
  (message "Copied current line."))

(defun try-to-load-elisp (elisp-file)
  "Try to call load-file on elisp-file.
If the file cannot be loaded, display an error message."
  (condition-case ex
      (load-file elisp-file)
    ('error (message "%s" ex))))

(defun load-lisp-in-dir (dir)
  "Load all elisp files (ending in .el extension) in dir and all subdirectories.
If there is an issue reading dir, display an error message."
  (if (file-exists-p dir)
      (condition-case ex
          (mapc 'try-to-load-elisp (directory-files-recursively dir "\\.el$"))
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

(defun create-new-buffer ()
  (interactive)
  (let ((new-buf (generate-new-buffer "new")))
    (switch-to-buffer new-buf)
    (set-buffer-major-mode new-buf)))

(defun dup-buffer ()
  (interactive)
  (let* ((curr-buf (current-buffer))
         (new-buf (generate-new-buffer (buffer-name curr-buf))))
    (switch-to-buffer new-buf)
    (insert-buffer curr-buf)))

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

(defun sudo-conn ()
  "Open a connection to the same machine, through sudo"
  (interactive)
  (let* ((tramp-end (cl-search ":" default-directory :from-end t))
         (new-path (if (not tramp-end) "/sudo::"
                     (format "%s|sudo::" (substring default-directory 0 tramp-end)))))
    (find-file new-path)))

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
      ;; Catch any errors. Whenever the user tries to scroll off the edges of a
      ;; buffer, Emacs treats this as an error and will exit window browsing.
      ;; This behavior is undesired; instead, the window browsing session should
      ;; continue.
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

(defun directory-for-background-command (prefix-arg command-buffer)
  "A helper function to determine the running directory for the
run-async-shell-command command."
  (cond
   ((and prefix-arg (listp prefix-arg))
    (read-directory-name "Working directory: "))
   ((get-buffer command-buffer)
    (with-current-buffer command-buffer default-directory))
   (t default-directory)))

(defun background-command-buffer (command)
  "Helper function to determine the command buffer for run-background-command."
  (format "*async %s*" command))

(defun run-background-command (command directory command-buffer)
  "Helper function for run-async-shell-command.
This defines the actual command logic."
  (let ((default-directory directory))
    (with-environment-variables (("PAGER" "cat"))
      (async-shell-command command command-buffer))))

(defun run-async-shell-command (prefix-arg command)
  "Run an asynchronous command.

The prefix argument controls the working directory for the command:
- No prefix: Use the last working directory of the command if its
  buffer still exists; otherwise, use the current buffer's directory.
- C-u one or more times: Prompt for a directory."
  (interactive (list current-prefix-arg
                     (read-shell-command "Command: " nil nil)))
  (let ((command-buffer (background-command-buffer command)))
    (run-background-command
     command
     (directory-for-background-command prefix-arg command-buffer)
     command-buffer)))

(defun loose-isearch-forward ()
  "Call isearch-forward, but with spaces in the search string matching one or
more of any character."
  (interactive)
  (let ((search-whitespace-regexp ".+"))
    (isearch-forward)))

(defun loose-isearch-backward ()
  "Call isearch-backward, but with spaces in the search string matching one or
more of any character."
  (interactive)
  (let ((search-whitespace-regexp ".+"))
    (isearch-backward)))

(defun save-filename-full-path ()
  "Save the file name of the current buffer, with the full path, to the kill
ring. Nothing is saved to the kill ring if the current buffer has no file
associated with it."
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (progn
          (message fname)
          (kill-new fname))
      (message "No file associated with this buffer."))))

(defun cleanup-deleted-files ()
  "Clean up buffers whose associated files have been deleted."
  (interactive)
  (mapc
   (lambda (buffer)
     (let ((buffer-file (buffer-file-name buffer)))
       (when (and buffer-file
                  (not (file-exists-p buffer-file)))
         (message "Cleaned up %s (file %s)" (buffer-name buffer) buffer-file)
         (kill-buffer buffer))))
   (buffer-list)))

(defun goto-scratch ()
  (interactive)
  (let ((default-directory "~/"))
    (switch-to-buffer "*scratch*")))

;; ============================================================================
;; Hooks and mode-specific setup
;; ============================================================================

(defvar my-minor-mode-map (make-sparse-keymap))

(define-minor-mode my-minor-mode
  "A minor mode to hold all of my global custom keybindings."
  :keymap my-minor-mode-map
  :init-value t
  :global t)

(my-minor-mode)

;; Frame hook setup
;; This defines a hook that will be run whenever a frame is created, or when
;; emacs is not started as a daemon.
(defun frame-creation-hook (frame)
  (with-selected-frame frame
    (when (not (eq (framep frame) t))
      (tool-bar-mode -1)
      (mouse-wheel-mode t)
      (set-scroll-bar-mode 'right)
      (if using-dark-theme (use-dark-theme)
        (use-light-theme))
      (condition-case ex
          (set-frame-font "DejaVu Sans Mono")
        ('error t)))))

(setq using-dark-theme t)

(defun use-dark-theme ()
  (interactive)
  (setq hl-line-color "#3E3D32")
  (set-face-background 'hl-line hl-line-color)
  (set-foreground-color "#DFDFD9")
  (set-background-color "#272822")
  (set-cursor-color "#FFFFFF")
  (setq using-dark-theme t))

(defun use-light-theme ()
  (interactive)
  (setq hl-line-color "cyan")
  (set-face-background 'hl-line hl-line-color)
  (set-foreground-color "black")
  (set-background-color "white")
  (set-cursor-color "black")
  (setq using-dark-theme nil))

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
           c-mode
           groovy-mode)))
    (when (member major-mode delete-whitespace-major-modes)
      (delete-trailing-whitespace))))

(defun linum-and-hl-line-hook ()
  "Defines a base mode hook that enables linum and hl-line minor modes.
There are many modes on which these should be enabled, so instead of defining a
function for each and then adding the mode hook separately, this function can
be applied to each major mode in a smarter way."
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode)
    (linum-mode))
  (highlight-line-mode)
  (electric-pair-local-mode))

(defun linum-spacing-on-terminal ()
  "A hook for determining linum-format.
In terminal buffers, there is no fringe between the line numbers and the buffer
content. To compensate for this, whenever a file is opened in a non-graphical
display, set the format string so that there is a space after each number."
  (when (not (display-graphic-p))
    (set (make-local-variable 'linum-format) "%d ")))

(add-hook 'find-file-hook 'linum-spacing-on-terminal)

(setq modes-for-linum-and-hl-line
      '(c++-mode-hook
        c-mode-hook
        conf-mode-hook
        css-mode-hook
        dockerfile-mode-hook
        emacs-lisp-mode-hook
        go-mode-hook
        go-dot-mod-mode-hook
        groovy-mode-hook
        java-mode-hook
        js-mode-hook
        lisp-mode-hook
        makefile-mode-hook
        org-mode-hook
        perl-mode-hook
        python-mode-hook
        racket-mode-hook
        ruby-mode-hook
        rust-mode-hook
        sh-mode-hook
        sql-mode-hook
        text-mode-hook
        web-mode-hook
        yaml-mode-hook))

(mapc (lambda (mode-name) (add-hook mode-name 'linum-and-hl-line-hook))
      modes-for-linum-and-hl-line)

(defun c-common-hook ()
  "A hook used for C/C++.
Start with the built-in linux mode and change things from there."
  (c-set-style "linux")
  (c-set-offset 'innamespace 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'inline-open 0)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(defun c++-hook ()
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file))

(defun c-hook ()
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file))

(defun diff-hook ()
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode)
    (linum-mode))
  (diff-auto-refine-mode -1))

(defun org-hook ()
  (org-indent-mode)
  (setq org-log-done "time")
  (local-set-key
   (kbd "C-c o")
   (if (version< emacs-version "25.1")
       'show-all 'outline-show-all)))

(defun all-terms-hook ()
  ;; Change ansi-color-blue and ansi-color-red to colors that are easier to
  ;; read.
  (set-face-attribute 'ansi-color-blue nil
                      :foreground "SkyBlue"
                      :background "SkyBlue")
  (set-face-attribute 'ansi-color-red nil
                      :foreground "Orchid"
                      :background "Orchid"))

(defun large-file-hook ()
  "Settings for large files."
  (when (> (buffer-size) (* 1024 1024))
    (buffer-disable-undo)
    (fundamental-mode)
    (when (fboundp 'linum-mode)
      (linum-mode -1))
    (read-only-mode)))

(defun shr-enable-images ()
  "Enable images for modes using shr.el"
  (interactive)
  (setq shr-inhibit-images nil))

(defun shr-disable-images ()
  "Disable images for modes using shr.el"
  (interactive)
  (setq shr-inhibit-images t))

(defun eww-hook ()
  "Settings for the eww browser."
  (setq shr-inhibit-images t)
  (define-key eww-mode-map (kbd "<prior>") 'small-scroll-down)
  (define-key eww-mode-map (kbd "<next>") 'small-scroll-up))

(defun python-hook ()
  "Settings for python mode."
  (setq python-indent-offset 4))

(defun dired-hook ()
  "Settings for dired mode"
  (setq dired-actual-switches "-lah"))

(defun go-hook ()
  "Settings for Go mode"
  (setq tab-width 4))

(defun proced-hook ()
  "Settings for proced"
  (setq proced-format 'medium)
  (visual-line-mode -1)
  (toggle-truncate-lines t))

(add-hook 'c-mode-common-hook 'c-common-hook)
(add-hook 'c++-mode-hook 'c++-hook)
(add-hook 'c-mode-hook 'c-hook)
(add-hook 'diff-mode-hook 'diff-hook)
(add-hook 'org-mode-hook 'org-hook)
(add-hook 'term-mode-hook 'all-terms-hook)
(add-hook 'comint-mode-hook 'all-terms-hook)
(add-hook 'vterm-mode-hook 'all-terms-hook)
(add-hook 'before-save-hook 'before-save)
(add-hook 'find-file-hook 'large-file-hook)
(add-hook 'eww-mode-hook 'eww-hook)
(add-hook 'python-mode-hook 'python-hook)
(add-hook 'dired-mode-hook 'dired-hook)
(add-hook 'go-mode-hook 'go-hook)
(add-hook 'proced-mode-hook 'proced-hook)

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

;; Use conf-mode for rc and yang files
(add-to-list 'auto-mode-alist '("rc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("yang\\'" . conf-mode))

;; Open C/C++ header files in c++-mode instead of c-mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defadvice shr-color-check (before shr-color-inhibit)
  "This advice tells eww to use the default foreground and background colors."
  (setq bg (face-background 'default))
  (setq fg (face-foreground 'default)))

;; ============================================================================
;; Project management
;; ============================================================================

(defun load-project-management ()
  (interactive)
  (if (and (require 'ivy nil 'noerror)
           (require 'projectile nil 'noerror))
      (progn
        (ivy-mode)
        (projectile-mode)
        (set-additional-project-settings)
        (set-additional-project-keys))
    (message "Project management not installed, skipping init")))

(defun determine-projectile-search-program ()
  (cond
   ((executable-find "rg") 'projectile-ripgrep)
   ((executable-find "ag") 'projectile-ag)
   (t 'projectile-grep)))

(defun find-file-default-completion ()
  "Use default completion for find-file.
For example, if ivy is enabled, this function will call find-file with ivy
temporarily disabled."
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (call-interactively 'find-file)))

(defun set-additional-project-keys ()
  (define-key my-minor-mode-map (kbd "C-c h") (determine-projectile-search-program))
  (define-key my-minor-mode-map (kbd "C-c p p") 'projectile-switch-project)
  (define-key my-minor-mode-map (kbd "C-c p f") 'projectile-find-file)
  (define-key my-minor-mode-map (kbd "C-c p i") 'projectile-invalidate-cache)
  (define-key my-minor-mode-map (kbd "C-c p g") 'projectile-open-magit-status)
  (define-key my-minor-mode-map (kbd "C-c p d") 'projectile-open-top-level-directory)
  (define-key my-minor-mode-map (kbd "C-x c a") 'apropos)
  (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-backward-delete-char)
  (define-key my-minor-mode-map (kbd "C-c p w") 'projectile-mode))

(defun set-additional-project-settings ()
  "Additional settings related to project management."
  (setq ivy-height 15
        ivy-initial-inputs-alist nil
        ivy-on-del-error-function nil
        ivy-dynamic-exhibit-delay-ms 250
        projectile-track-known-projects-automatically nil))

(defun projectile-open-magit-status ()
  "Run magit-status on a known projectile project."
  (interactive)
  (let ((default-directory
         (completing-read "Select a project: " projectile-known-projects)))
    (magit-status)))

(defun projectile-open-top-level-directory ()
  "Open the top level directory of a known projectile project using dired."
  (interactive)
  (let ((selected-project
         (completing-read "Select a project: " projectile-known-projects)))
    (dired selected-project)))

;; ============================================================================
;; Global key bindings
;; ============================================================================

(define-key my-minor-mode-map [f1] 'search-all-buffers)
(define-key my-minor-mode-map [f2] 'revert-buffer)
(define-key my-minor-mode-map [f5] 'reload-emacs-config)
(define-key my-minor-mode-map [f6] 'compile)
(define-key my-minor-mode-map [f7] 'recompile)
(define-key my-minor-mode-map [f8] 'load-project-management)
(define-key my-minor-mode-map [f9] 'goto-scratch)
(define-key my-minor-mode-map [f12] 'tramp-cleanup-all)
(define-key my-minor-mode-map [(control shift delete)] 'delete-region)
(define-key my-minor-mode-map (kbd "C-x R") 'rename-buffer)
(define-key my-minor-mode-map (kbd "C-x ,") 'kill-matching-buffers)
(define-key my-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key my-minor-mode-map (kbd "C-S-a") 'back-to-indentation)
(define-key my-minor-mode-map (kbd "C-c C-h") 'highlight-all-current-region)
(define-key my-minor-mode-map (kbd "M-N") 'create-new-buffer)
(define-key my-minor-mode-map (kbd "M-P") 'window-browser)
(define-key my-minor-mode-map (kbd "C-c s") 'eshell)
(define-key my-minor-mode-map (kbd "C-c a") 'eshell-cd-to-current-directory)
(define-key my-minor-mode-map (kbd "C-x j") 'join-line)
(define-key my-minor-mode-map (kbd "C-{") 'previous-buffer)
(define-key my-minor-mode-map (kbd "C-}") 'next-buffer)
(define-key my-minor-mode-map (kbd "C-<") 'small-scroll-down)
(define-key my-minor-mode-map (kbd "C->") 'small-scroll-up)
(define-key my-minor-mode-map (kbd "C-\"") 'other-window)
(define-key my-minor-mode-map (kbd "C-:") 'move-backwards)
(define-key my-minor-mode-map (kbd "C-c w") 'whitespace-mode)
(define-key my-minor-mode-map (kbd "M-W") 'copy-line-at-indentation)
(define-key my-minor-mode-map (kbd "M-g v") 'magit-status)
(define-key my-minor-mode-map (kbd "M-g d") 'magit-diff-range)
(define-key my-minor-mode-map (kbd "M-g b") 'magit-blame-addition)
(define-key my-minor-mode-map (kbd "M-g l") 'magit-log-buffer-file)
(define-key my-minor-mode-map (kbd "M-S") 'loose-isearch-forward)
(define-key my-minor-mode-map (kbd "M-R") 'loose-isearch-backward)
(define-key my-minor-mode-map (kbd "C-x S") 'save-filename-full-path)
(define-key my-minor-mode-map (kbd "C-c d") 'kill-whole-line)
(define-key my-minor-mode-map (kbd "C-x C-j") 'dired-jump)
(define-key my-minor-mode-map (kbd "C-x ;") 'comment-line)
(define-key my-minor-mode-map (kbd "C-x F") 'find-file-default-completion)
(define-key my-minor-mode-map (kbd "M-&") 'run-async-shell-command)
(define-key my-minor-mode-map (kbd "M-o a") 'org-agenda)
(define-key my-minor-mode-map (kbd "M-g s") 'git-quick-status)
(define-key my-minor-mode-map (kbd "C-M-y") 'xref-pop-marker-stack)
(define-key my-minor-mode-map (kbd "C-c $") 'toggle-truncate-lines)
(define-key my-minor-mode-map (kbd "C-M-+") 'bump-font-size-up)
(define-key my-minor-mode-map (kbd "C-M--") 'bump-font-size-down)
(define-key my-minor-mode-map (kbd "C-M-0") 'bump-default-font-size)

(defun dssh (ssh-params)
  "Open a dired session to a remote host."
  (interactive
   (list
    (completing-read "Enter SSH host: " (get-ssh-config-hosts))))
  (dired (format "/ssh:%s:" ssh-params)))

(when (require 'vterm nil 'noerror)
  (defun vterm-new-term ()
    (interactive)
    (let* ((prefix-val (car current-prefix-arg))
           (default-directory
            (cond ((eql prefix-val 4) ;; C-u pressed once
                   (getenv "HOME"))
                  ((eql prefix-val 16) ;; C-u pressed twice
                   (read-directory-name "Enter starting directory: "))
                  (t default-directory)))) ;; Any other prefix (including none)
      (vterm t)))
  (defun vssh (ssh-params)
    (interactive
     (list
      (completing-read "Enter SSH host: " (get-ssh-config-hosts))))
    (let ((vterm-buffer-name (format "*vterm ssh %s*" ssh-params))
          (default-directory (getenv "HOME")))
      (vterm t)
      (vterm-insert (format "exec bash -c 'if ! ssh %s; then read -n 1 -r -s -p \"Press any key to continue...\"; fi'\n" ssh-params))
      (setq default-directory (format "/ssh:%s:" ssh-params))))
  (defun vbash ()
    (interactive)
    (vterm-insert "exec bash -il\n"))
  (define-key my-minor-mode-map (kbd "C-c C-v") 'vterm-new-term)
  (defun vterm-default-directory()
    (interactive)
    (vterm-insert (format "cd %s\n" default-directory)))
  (define-key vterm-mode-map (kbd "C-c C-d") 'vterm-default-directory)
  (setq vterm-max-scrollback 100000))

(when (and (require 'vterm nil 'noerror)
           (file-exists-p (or (getenv "KUBECONFIG") "~/.kube/config"))
           (require 'kubel nil 'noerror))
  (kubel-vterm-setup))

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
;; Eglot configuration and definitions
;; ============================================================================

(defun eglot-organize-imports ()
  "Action to organize imports in the current buffer's file."
  (interactive)
  (eglot-code-actions (point-min) (point-max) "source.organizeImports" t))

(defun setup-eglot()
  "Define keybindings and update symbol highlighting for eglot."
  (define-key my-minor-mode-map (kbd "M-' r r") 'eglot-rename)
  (define-key my-minor-mode-map (kbd "M-' g i") 'eglot-find-implementation)
  (define-key my-minor-mode-map (kbd "M-' r o") 'eglot-organize-imports)
  (define-key my-minor-mode-map (kbd "M-' g g") 'xref-find-definitions)
  (define-key my-minor-mode-map (kbd "M-' g r") 'xref-find-references)
  (define-key my-minor-mode-map (kbd "M-' a a") 'eglot-code-actions)
  (define-key my-minor-mode-map (kbd "M-' r f") 'eglot-format-buffer)
  (define-key my-minor-mode-map (kbd "M-' g t") 'eglot-find-typeDefinition)
  (define-key my-minor-mode-map (kbd "M-' w r") 'eglot-reconnect)
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'highlight)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (put 'eglot-error 'flymake-overlay-control nil)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(defun eglot-managed-hook ()
  "Hook for eglot mode setup.
Add eglot-ensure as a major mode hook to enable eglot."
  (company-mode))

(add-hook 'eglot-managed-mode-hook 'eglot-managed-hook)

(with-eval-after-load "eglot" (setup-eglot))


;; ============================================================================
;; Package management
;; ============================================================================

;; The list of packages to install when calling install-selected-packages.
(setq packages-to-install
      '(company
        dockerfile-mode
        erc-hl-nicks
        go-mode
        groovy-mode
        ivy
        jinja2-mode
        kubel
        magit
        markdown-mode
        projectile
        projectile-ripgrep
        protobuf-mode
        rust-mode
        swiper
        undo-tree
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

(defun install-selected-packages ()
  "Ensure that all packages in the list \"packages-to-install\" are installed."
  (interactive)
  (package-refresh-contents)
  (install-packages-if-not-installed packages-to-install))

;; Load local lisp files last. This allows local settings to override settings
;; made in .emacs.
(defvar reload-elisp t)
(when reload-elisp
  (load-lisp-in-dir "~/elisp"))
