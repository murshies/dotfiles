;; ============================================================================
;; Miscellaneous settings
;; ============================================================================

(blink-cursor-mode -1)
(column-number-mode)
(delete-selection-mode)
(show-paren-mode)
(eval-after-load "linum" '(set-face-attribute 'linum nil :height 120))
(fset 'yes-or-no-p 'y-or-n-p)
(global-hi-lock-mode)
(global-visual-line-mode)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(set-face-attribute 'default nil :height 120)
;; Tell Emacs to automatically place the point at the end of the compilation
;; buffer.
(setq compilation-scroll-output t)
(setq create-lockfiles nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq erc-join-buffer 'bury)
(setq frame-title-format
      '(:eval (if buffer-file-name "%b (%f)" "%b")))
(setq inhibit-startup-screen t)
(setq ivy-height 15)
(setq ivy-initial-inputs-alist nil)
(setq ivy-on-del-error-function nil)
(setq org-replace-disputed-keys t)
(setq org-todo-keyword-faces '(("TODO" . hi-yellow)))
(setq ring-bell-function 'ignore)
;; Scroll when we're 2 row away from the edge of the window.
(setq smooth-scroll-margin 2)
(setq tags-add-tables nil)
(setq-default indent-tabs-mode nil)
(setq-default major-mode 'text-mode)
(setq-default fill-column 79)
(when (require 'package nil 'noerror)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (setq custom-file "~/.emacs.d/custom.el"))
;; Note: loading features that were downloaded from the package manager must be
;; done after calling package-initialize.
(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode))

;; eshell settings
(defun eshell/e (file &rest files)
  "Open a list of files.
This function is meant to be called as a command in eshell. Wildcards are
supported."
  (let* ((unflattened (append (list file) files))
         (flattened (eshell-flatten-list unflattened))
         (full-paths (mapcar (lambda (f)
                               (concat default-directory f))
                             flattened)))
    (mapc #'find-file full-paths)))

(defun eshell/a (cmd &rest args)
  "Runs an asynchronous command.
This is the same as running run-async-shell-command with no prefix argument and
the default directory as the eshell buffer's default directory."
  (let ((full-cmd (string-join (cons cmd args) " ")))
    (run-background-command
     full-cmd
     default-directory
     (background-command-buffer full-cmd))))

(defun colorize-eshell-prompt (prompt)
  "Apply colors to the eshell prompt."
  (concat
   (propertize prompt 'face '(:foreground "#00CC00"))
   (propertize " " 'face '(:foreground "#FFFFFF"))))

;; eshell custom prompt
(setq eshell-prompt-function
      (lambda ()
        (colorize-eshell-prompt
         (concat
          "\n["
          user-login-name
          "@"
          system-name
          " "
          (eshell/pwd)
          "]\n"
          (if (= (user-uid) 0) "#" "$")))))
(setq eshell-prompt-regexp "[#$] ")
(setq eshell-highlight-prompt nil)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

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
         (extra-ssh-args "-o ServerAliveInterval=5 -o ServerAliveCountMax=2 ")
         (ssh-command (concat "ssh " extra-ssh-args ssh-args))
         (full-command (concat "while [ 1 ]; do "
                               ssh-command
                               " ; sleep 1 ; done")))
    (ansi-term "/bin/bash" buffer-name)
    (switch-to-buffer full-buffer-name)
    (insert full-command)
    (term-send-input)))

(defun highlight-line-mode ()
  (if (display-graphic-p)
      (progn
        (hl-line-mode t)
        (set-face-background 'hl-line hl-line-color))))

(defun copy-line-at-indentation ()
  "Copies the current line to the kill ring, excluding indentation."
  (interactive)
  (kill-ring-save (+ (line-beginning-position) (current-indentation))
                  (line-end-position))
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

(defun matches-any-regex (regex-list str)
  (if (not regex-list) nil
    (let ((next-regex (car regex-list)))
      (if (string-match-p next-regex str) t
        (matches-any-regex (cdr regex-list) str)))))

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

(defun term-cd-to-starting-directory ()
  "Change the current directory in the current term-mode buffer to its starting
buffer. This relies on the current term-mode buffer having a variable defined
called starting-directory."
  (interactive)
  (insert (concat "cd " starting-directory))
  (term-send-input))

(defun get-ansi-shell-command ()
  "A helper function for handling the logic of determining which shell should
be used for ansi-term buffers."
  (let ((default-shell "/bin/bash"))
    (if (boundp 'explicit-shell-file-name)
        (or explicit-shell-file-name default-shell)
      default-shell)))

(defun named-ansi-term (program)
  "Creates an ansi-term buffer with a customized name. The format of the name
is *ansi-<current directory>*."
  (interactive
   (list
    (let ((shell-to-use (get-ansi-shell-command)))
      (read-string "Run program: " shell-to-use nil
                   shell-to-use))))
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
  "Generate an etags file in a directory specified by the user.
The user also specifies a pattern (passed to find) that will match against
files in the specified directory and subdirectories."
  (interactive
   (list
    (read-directory-name "Enter the etags root: ")
    (read-regexp "Enter any number of file name patterns, separated by spaces: ")))
  (let* ((tag-file (concat (file-name-as-directory directory) "TAGS"))
         (file-pattern-list
          (cl-remove-if (lambda (s) (= (length s) 0))
                        (split-string file-patterns)))
         (find-file-string
          (mapconcat 'identity file-pattern-list "\" -or -name \""))
         (etags-command
          (format "find \"%s\" -type f -and \\( -name \"%s\" \\) | etags -f %s -"
                  directory find-file-string tag-file)))
    (compilation-start etags-command nil
                       (lambda (_) (format "*etags [%s %s]*"
                                           directory file-patterns)))))

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
    (async-shell-command command command-buffer)))

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

(defun toggle-pin-buffer-to-window ()
  "If the current window is not dedicated to its buffer, dedicate it and vice
versa."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) (not (window-dedicated-p)))
  (message (format "Window is%s dedicated to its buffer"
                   (if (window-dedicated-p) "" " not"))))

(defun toggle-close-confirm ()
  "Toggle emacs close confirmation behavior. Enable a yes or no prompt when
calling save-buffers-kill-terminal if it would exit immediately and vice
versa."
  (interactive)
  (if confirm-kill-emacs
      (progn
        (setq confirm-kill-emacs nil)
        (message "Close confirmation disabled."))
    (progn
      (setq confirm-kill-emacs 'yes-or-no-p)
      (message "Close confirmation enabled."))))

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

;; ============================================================================
;; Hooks and mode-specific setup
;; ============================================================================

(defvar my-minor-mode-map (make-sparse-keymap))

(define-minor-mode my-minor-mode
  "A minor mode to hold all of my global custom keybindings."
  :keymap my-minor-mode-map)

(define-global-minor-mode
  my-global-minor-mode
  my-minor-mode
  (lambda () (my-minor-mode)))

(my-global-minor-mode)

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
  (set-foreground-color "#DFDFD9")
  (set-background-color "#272822")
  (set-cursor-color "#FFFFFF")
  (setq using-dark-theme t))

(defun use-light-theme ()
  (interactive)
  (setq hl-line-color "cyan")
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
           c-mode)))
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
  (highlight-line-mode))

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
        clojure-mode-hook
        conf-mode-hook
        css-mode-hook
        emacs-lisp-mode-hook
        go-mode-hook
        js-mode-hook
        lua-mode-hook
        lisp-mode-hook
        makefile-mode-hook
        org-mode-hook
        perl-mode-hook
        python-mode-hook
        racket-mode-hook
        ruby-mode-hook
        rust-mode-hook
        sh-mode-hook
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
  (linum-mode)
  (diff-auto-refine-mode -1))

(defun eshell-hook ()
  (setq pcomplete-cycle-completions nil))

(defun org-hook ()
  (org-indent-mode)
  (setq org-log-done "time")
  (local-set-key
   (kbd "C-c o")
   (if (version< emacs-version "25.1")
       'show-all 'outline-show-all)))

(defun lua-hook ()
  (setq lua-indent-level 4))

(defun term-hook ()
  ;; The default blue is incredibly difficult to read
  (set-face-attribute 'term-color-blue nil :foreground "SkyBlue")
  (set-face-attribute 'term-color-red nil :foreground "Orchid")
  (make-local-variable 'starting-directory)
  (setq starting-directory default-directory)
  (define-key term-mode-map (kbd "C-c g") 'term-cd-to-starting-directory)
  (define-key term-raw-map (kbd "C-c g") 'term-cd-to-starting-directory)
  (define-key term-raw-map (kbd "C-c s") nil)
  (define-key term-raw-map (kbd "M-x")
    (lookup-key (current-global-map) (kbd "M-x")))
  (define-key term-raw-map (kbd "M-:") 'eval-expression)
  (define-key term-raw-map (kbd "M-P") 'window-browser))

(defun clojure-hook ()
  (define-key clojure-mode-map (kbd "C-'") 'clojure-toggle-keyword-string)
  (define-key clojure-mode-map (kbd "C-:") 'move-backwards))

(defun large-file-hook ()
  "Settings for large files."
  (when (> (buffer-size) (* 1024 1024))
    (buffer-disable-undo)
    (fundamental-mode)
    (linum-mode -1)))

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

(add-hook 'c-mode-common-hook 'c-common-hook)
(add-hook 'c++-mode-hook 'c++-hook)
(add-hook 'c-mode-hook 'c-hook)
(add-hook 'clojure-mode-hook 'clojure-hook)
(add-hook 'diff-mode-hook 'diff-hook)
(add-hook 'eshell-mode-hook 'eshell-hook)
(add-hook 'org-mode-hook 'org-hook)
(add-hook 'lua-mode-hook 'lua-hook)
(add-hook 'term-mode-hook 'term-hook)
(add-hook 'before-save-hook 'before-save)
(add-hook 'find-file-hook 'large-file-hook)
(add-hook 'eww-mode-hook 'eww-hook)
(add-hook 'python-mode-hook 'python-hook)
(add-hook 'dired-mode-hook 'dired-hook)
(add-hook 'go-mode-hook 'go-hook)

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

;; Define key bindings for python-mode when it is loaded. This is usually done
;; when the first python file of the emacs session is opened.
(eval-after-load "python"
  '(progn (define-key python-mode-map (kbd "C-c v f") 'pyflakes-current-file)
          (define-key python-mode-map (kbd "C-c v l") 'pylint-current-file)
          (define-key python-mode-map (kbd "C-c C-b") 'python-run-current-buffer)))

(defun pyflakes-current-file ()
  "Run pyflakes on the current file.
Even though the documentation for python-check says that the default file is
supposed to be the current buffer's default, this isn't true after the first
time that pyflakes it run. This function fixes this issue."
  (interactive)
  (python-check (concat "pyflakes \"" buffer-file-name "\"")))

(defun pylint-current-file ()
  "Run pylint on the current file.
This runs in the same way as pyflakes-current-file (inside a compilation-mode
buffer), but with pylint instead. It will use the default .pylintrc file."
  (interactive)
  (python-check (concat "pylint -f text \"" buffer-file-name "\"")))

(defun python-run-current-buffer ()
  (interactive)
  (save-selected-window
    (python-shell-send-buffer)
    (python-shell-switch-to-shell)
    (end-of-buffer)
    (insert "main()")
    (comint-send-input)))

;; Similar to the python-mode checks, only define keybindings that are specific
;; to yaml-mode after it has been loaded.
(eval-after-load "yaml"
  '(define-key yaml-mode-map (kbd "C-c v l") 'ansible-lint-current-file))

(defun ansible-lint-current-file ()
  "Runs an ansible syntax-check on the current file."
  (interactive)
  (let* ((command (concat "ansible-playbook --syntax-check \""
                          buffer-file-name
                          "\""))
         (buffer-name (concat "*" command "*")))
    (compilation-start command nil (lambda (_modename) buffer-name))))

(defadvice shr-color-check (before shr-color-inhibit)
  "This advice tells eww to use the default foreground and background colors."
  (setq bg (face-background 'default))
  (setq fg (face-foreground 'default)))

;; ============================================================================
;; Project management
;; ============================================================================

(defun load-project-management ()
  (interactive)
  (ivy-mode)
  (set-additional-project-keys))

(defun determine-projectile-search-program ()
  (cond
   ((executable-find "rg") 'counsel-projectile-rg)
   ((executable-find "ag") 'counsel-projectile-ag)
   (t 'counsel-projectile-grep)))

(defun find-file-default-completion ()
  "Use default completion for find-file.
For example, if ivy is enabled, this function will call find-file with ivy
temporarily disabled."
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (call-interactively 'find-file)))

(defun set-additional-project-keys ()
  (define-key my-minor-mode-map (kbd "C-c h") (determine-projectile-search-program))
  (define-key my-minor-mode-map (kbd "C-c p p") 'counsel-projectile-switch-project)
  (define-key my-minor-mode-map (kbd "C-c p f") 'counsel-projectile-find-file)
  (define-key my-minor-mode-map (kbd "C-c p i") 'projectile-invalidate-cache)
  (define-key my-minor-mode-map (kbd "C-x c a") 'counsel-apropos)
  (define-key my-minor-mode-map (kbd "M-.") 'counsel-etags-find-tag-at-point)
  (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-backward-delete-char)
  (define-key my-minor-mode-map (kbd "M-x") 'counsel-M-x)
  (define-key my-minor-mode-map (kbd "C-c p w") 'counsel-projectile-mode)
  (define-key my-minor-mode-map (kbd "C-.") 'counsel-etags-list-tag)
  (define-key my-minor-mode-map [f9] 'counsel-etags-update-tags-force))

;; ============================================================================
;; Global key bindings
;; ============================================================================

(define-key my-minor-mode-map [f1] 'search-all-buffers)
(define-key my-minor-mode-map [f2] 'revert-buffer)
(define-key my-minor-mode-map [f5] 'reload-emacs-config)
(define-key my-minor-mode-map [f6] 'compile)
(define-key my-minor-mode-map [f7] 'recompile)
(define-key my-minor-mode-map [f8] 'load-project-management)
(define-key my-minor-mode-map [f12] 'tramp-cleanup-all)
(define-key my-minor-mode-map [(control shift delete)] 'delete-region)
(define-key my-minor-mode-map (kbd "C-x R") 'rename-buffer)
(define-key my-minor-mode-map (kbd "C-x ,") 'kill-matching-buffers)
(define-key my-minor-mode-map (kbd "C-x C-b") 'ibuffer)
(define-key my-minor-mode-map (kbd "C-S-a") 'back-to-indentation)
(define-key my-minor-mode-map (kbd "C-c C-h") 'highlight-all-current-region)
(define-key my-minor-mode-map (kbd "M-N") 'create-new-buffer)
(define-key my-minor-mode-map (kbd "M-P") 'window-browser)
(define-key my-minor-mode-map (kbd "C-c s e") 'eshell)
(define-key my-minor-mode-map (kbd "C-c s d") 'eshell-cd-to-current-directory)
(define-key my-minor-mode-map (kbd "C-c s a") 'named-ansi-term)
(define-key my-minor-mode-map (kbd "C-c s s") 'ssh-ansi)
(define-key my-minor-mode-map (kbd "C-c s w") 'ansi-term-in-directory)
(define-key my-minor-mode-map (kbd "M-I") 'windmove-up)
(define-key my-minor-mode-map (kbd "M-K") 'windmove-down)
(define-key my-minor-mode-map (kbd "M-J") 'windmove-left)
(define-key my-minor-mode-map (kbd "M-L") 'windmove-right)
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
(define-key my-minor-mode-map (kbd "C-x p") 'toggle-pin-buffer-to-window)
(define-key my-minor-mode-map (kbd "C-x S") 'save-filename-full-path)
(define-key my-minor-mode-map (kbd "C-c d") 'kill-whole-line)
(define-key my-minor-mode-map (kbd "C-a") 'beginning-of-line)
(define-key my-minor-mode-map (kbd "C-e") 'end-of-line)
(define-key my-minor-mode-map (kbd "C-x C-j") 'dired-jump)
(define-key my-minor-mode-map (kbd "C-x ;") 'comment-line)
(define-key my-minor-mode-map (kbd "C-x F") 'find-file-default-completion)
(define-key my-minor-mode-map (kbd "M-&") 'run-async-shell-command)

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
      '(counsel-etags
        counsel-projectile
        dockerfile-mode
        erc-hl-nicks
        go-mode
        ivy
        jinja2-mode
        magit
        markdown-mode
        rust-mode
        rust-playground
        smooth-scrolling
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
(when (or (not (boundp 'reload-elisp)) reload-elisp)
  (load-lisp-in-dir "~/elisp"))
