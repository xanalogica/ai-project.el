;;; ai-project.el --- project.el helpers for AI coding agents  -*- lexical-binding: t; -*-

;; Author: Jeffrey Rush
;; Maintainer: Jeffrey Rush
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (vterm "0.0"))
;; Keywords: project, tools, ai, convenience
;; URL: https://github.com/xanalogica/ai-project.el

;;; Commentary:

;; ai-project.el extends project.el for AI-oriented project workflows.
;;
;; Features:
;; - remembers visited projects
;; - optional project indicator in the mode line
;; - project file selection sorted by name
;; - project-rooted shells and AI tool launchers
;; - per-project credentials via auth-source / ~/.authinfo.gpg
;; - safe local variables for .dir-locals.el
;; - dedicated launch commands plus a dispatcher
;; - vterm buffer reuse when tool + directory + effective command/env match
;;
;; Expected .dir-locals.el variables:
;;
;;   ((nil . ((ai-project-llm-project-key . "TheAccountant")
;;            (ai-project-claude-args . "--model claude-sonnet-4")
;;            (ai-project-codex-args . "--model gpt-5-codex")
;;            (ai-project-ant-args . "--model some-ant-mode"))))
;;
;; Credential lookup in ~/.authinfo.gpg:
;;
;;   machine anthropic/TheAccountant login apikey password sk-ant-...
;;   machine openai/TheAccountant    login apikey password sk-proj-...
;;
;; Behavior:
;; - Shell launcher: missing AI keys are ignored.
;; - Claude / ant launcher: require Anthropic API key.
;; - Codex launcher: require OpenAI API key.

;;; Code:

(declare-function vterm "vterm" (&optional buffer-name))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-return "vterm" ())

(require 'project)
(require 'auth-source)
(require 'subr-x)
(require 'seq)

(defgroup ai-project nil
  "project.el helpers for AI coding agents."
  :group 'project
  :prefix "ai-project-")

(defcustom ai-project-enable-project-remembering t
  "When non-nil, remember visited projects from `find-file-hook'."
  :type 'boolean)

(defcustom ai-project-enable-mode-line t
  "When non-nil, show current project in the mode line."
  :type 'boolean)

(defcustom ai-project-project-list-file
  (expand-file-name "projects" user-emacs-directory)
  "File where project.el stores known projects."
  :type 'file)

(defcustom ai-project-shell-command nil
  "Shell command used by `ai-project-shell'.

If nil, use (getenv \"SHELL\") when available, otherwise fall back to \"bash\"."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Explicit shell command")))

(defcustom ai-project-vterm-buffer-format "*%s:%s*"
  "Format string for vterm buffer names.
Arguments are TOOL-NAME and DIRECTORY-LABEL."
  :type 'string)

(defcustom ai-project-key-fallback-to-directory-name t
  "When non-nil, fall back to the project directory name as the logical key."
  :type 'boolean)

(defcustom ai-project-key-file-name ".llm-project"
  "Optional file in project root containing the logical project key."
  :type 'string)

(defcustom ai-project-reuse-live-buffers t
  "When non-nil, reuse matching live vterm buffers for shells and AI tools."
  :type 'boolean)

(defcustom ai-project-dispatcher-tools
  '(shell claude codex ant)
  "Tool names offered by `ai-project-launch'."
  :type '(repeat (choice (const shell)
                         (const claude)
                         (const codex)
                         (const ant))))

(defvar-local ai-project-llm-project-key nil
  "Logical project key used to look up per-project LLM credentials.")

(defvar-local ai-project-claude-args nil
  "Additional command-line arguments for the `claude` executable.")

(defvar-local ai-project-codex-args nil
  "Additional command-line arguments for the `codex` executable.")

(defvar-local ai-project-ant-args nil
  "Additional command-line arguments for the `ant` executable.")

(put 'ai-project-llm-project-key 'safe-local-variable #'stringp)
(put 'ai-project-claude-args 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'ai-project-codex-args 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))
(put 'ai-project-ant-args 'safe-local-variable
     (lambda (x) (or (null x) (stringp x))))

(defun ai-project-root ()
  "Return the current project root.
Signal an error if current buffer is not inside a project."
  (if-let ((proj (project-current nil)))
      (expand-file-name (project-root proj))
    (error "Not inside a project")))

(defun ai-project-name ()
  "Return the current project's display name."
  (project-name (project-current t)))

(defun ai-project--key-from-file (root)
  "Return logical project key from ROOT/.llm-project, or nil."
  (let ((file (expand-file-name ai-project-key-file-name root)))
    (when (file-exists-p file)
      (let ((value (string-trim
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))
        (unless (string-empty-p value)
          value)))))

(defun ai-project--dir-local-settings (dir)
  "Return relevant AI dir-local settings for DIR as an alist."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (expand-file-name dir)))
    (delay-mode-hooks (fundamental-mode))
    (hack-dir-local-variables-non-file-buffer)
    `((ai-project-llm-project-key . ,ai-project-llm-project-key)
      (ai-project-claude-args . ,ai-project-claude-args)
      (ai-project-codex-args . ,ai-project-codex-args)
      (ai-project-ant-args . ,ai-project-ant-args))))

(defun ai-project--project-key-for-dir (dir)
  "Return the logical project key for DIR."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (settings (ai-project--dir-local-settings dir))
         (local-key (alist-get 'ai-project-llm-project-key settings))
         (file-key (ai-project--key-from-file dir)))
    (or (and (stringp local-key)
             (not (string-empty-p local-key))
             local-key)
        file-key
        (and ai-project-key-fallback-to-directory-name
             (file-name-nondirectory
              (directory-file-name dir)))
        (error "No AI project key defined for %s" dir))))

(defun ai-project-key ()
  "Return the logical project key for the current project."
  (ai-project--project-key-for-dir (ai-project-root)))

(defun ai-project--authinfo-secret-maybe (host)
  "Return the secret associated with HOST from auth-source, or nil."
  (let* ((result (car (auth-source-search
                       :host host
                       :max 1
                       :require '(:secret))))
         (secret (plist-get result :secret)))
    (when result
      (if (functionp secret)
          (funcall secret)
        secret))))

(defun ai-project-anthropic-key-maybe (&optional dir)
  "Return Anthropic API key for DIR/current project, or nil."
  (ai-project--authinfo-secret-maybe
   (format "anthropic/%s"
           (ai-project--project-key-for-dir (or dir (ai-project-root))))))

(defun ai-project-openai-key-maybe (&optional dir)
  "Return OpenAI API key for DIR/current project, or nil."
  (ai-project--authinfo-secret-maybe
   (format "openai/%s"
           (ai-project--project-key-for-dir (or dir (ai-project-root))))))

(defun ai-project-require-anthropic-key (&optional dir)
  "Return Anthropic API key for DIR/current project or signal an error."
  (or (ai-project-anthropic-key-maybe dir)
      (error "No Anthropic API key found for project %s"
             (ai-project--project-key-for-dir (or dir (ai-project-root))))))

(defun ai-project-require-openai-key (&optional dir)
  "Return OpenAI API key for DIR/current project or signal an error."
  (or (ai-project-openai-key-maybe dir)
      (error "No OpenAI API key found for project %s"
             (ai-project--project-key-for-dir (or dir (ai-project-root))))))

(defun ai-project-shell-env (&optional dir)
  "Return environment for a plain project shell in DIR/current project.
Missing AI keys are ignored."
  (let* ((dir (or dir (ai-project-root)))
         (env nil)
         (key (ai-project--project-key-for-dir dir)))
    (when-let ((anthropic (ai-project-anthropic-key-maybe dir)))
      (push (format "ANTHROPIC_API_KEY=%s" anthropic) env))
    (when-let ((openai (ai-project-openai-key-maybe dir)))
      (push (format "OPENAI_API_KEY=%s" openai) env))
    (push (format "LLM_PROJECT_NAME=%s" key) env)
    (nreverse env)))

(defun ai-project-claude-env (&optional dir)
  "Return environment for Claude Code in DIR/current project.
Require an Anthropic API key."
  (let ((dir (or dir (ai-project-root))))
    (list (format "ANTHROPIC_API_KEY=%s" (ai-project-require-anthropic-key dir))
          (format "LLM_PROJECT_NAME=%s" (ai-project--project-key-for-dir dir)))))

(defun ai-project-ant-env (&optional dir)
  "Return environment for Anthropic ant in DIR/current project.
Require an Anthropic API key."
  (let ((dir (or dir (ai-project-root))))
    (list (format "ANTHROPIC_API_KEY=%s" (ai-project-require-anthropic-key dir))
          (format "LLM_PROJECT_NAME=%s" (ai-project--project-key-for-dir dir)))))

(defun ai-project-codex-env (&optional dir)
  "Return environment for Codex in DIR/current project.
Require an OpenAI API key."
  (let ((dir (or dir (ai-project-root))))
    (list (format "OPENAI_API_KEY=%s" (ai-project-require-openai-key dir))
          (format "LLM_PROJECT_NAME=%s" (ai-project--project-key-for-dir dir)))))

(defun ai-project--resolve-shell-command ()
  "Return the shell command to use for project shells."
  (or ai-project-shell-command
      (let ((shell (getenv "SHELL")))
        (cond
         ((and shell (file-exists-p shell)) shell)
         ((executable-find "bash") "bash")
         (t (error "No suitable shell found"))))))

(defun ai-project--command-with-args (program args)
  "Return PROGRAM plus optional ARGS as a shell command string."
  (if (and (stringp args) (not (string-empty-p args)))
      (format "%s %s" program args)
    program))

(defun ai-project--tool-command-for-dir (tool dir)
  "Return the CLI command string for TOOL in DIR, or nil for plain shell."
  (let* ((settings (ai-project--dir-local-settings dir))
         (claude-args (alist-get 'ai-project-claude-args settings))
         (codex-args (alist-get 'ai-project-codex-args settings))
         (ant-args (alist-get 'ai-project-ant-args settings)))
    (pcase tool
      ('shell nil)
      ('claude (ai-project--command-with-args "claude" claude-args))
      ('codex  (ai-project--command-with-args "codex" codex-args))
      ('ant    (ai-project--command-with-args "ant" ant-args))
      (_ (error "Unknown ai-project tool: %S" tool)))))

(defun ai-project--tool-env-for-dir (tool dir)
  "Return startup environment for TOOL in DIR."
  (pcase tool
    ('shell  (ai-project-shell-env dir))
    ('claude (ai-project-claude-env dir))
    ('codex  (ai-project-codex-env dir))
    ('ant    (ai-project-ant-env dir))
    (_ (error "Unknown ai-project tool: %S" tool))))

(defun ai-project--tool-display-name (tool)
  "Return display name for TOOL."
  (symbol-name tool))

(defun ai-project--buffer-name-for-dir (tool dir)
  "Return a stable vterm buffer name for TOOL rooted at DIR."
  (format ai-project-vterm-buffer-format
          (ai-project--tool-display-name tool)
          (file-name-nondirectory
           (directory-file-name (file-name-as-directory dir)))))

(defun ai-project--normalize-env (env)
  "Return ENV sorted for stable comparison."
  (sort (copy-sequence env) #'string<))

(defun ai-project--launch-signature (tool dir command env)
  "Return a stable signature string for TOOL, DIR, COMMAND, and ENV."
  (prin1-to-string
   (list :tool tool
         :dir (file-name-as-directory (expand-file-name dir))
         :command command
         :env (ai-project--normalize-env env))))

(defun ai-project--live-process-p (buffer)
  "Return non-nil if BUFFER has a live process."
  (when-let ((proc (get-buffer-process buffer)))
    (process-live-p proc)))

(defun ai-project--matching-buffer-p (buffer signature)
  "Return non-nil if BUFFER is a live ai-project buffer matching SIGNATURE."
  (with-current-buffer buffer
    (and (ai-project--live-process-p buffer)
         (equal (bound-and-true-p ai-project--signature) signature))))

(defun ai-project--kill-buffer-if-live (buffer)
  "Kill BUFFER if it exists."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun ai-project--apply-dir-locals-for-buffer (dir)
  "Apply directory local variables for DIR to the current non-file buffer."
  (setq-local default-directory (file-name-as-directory (expand-file-name dir)))
  (hack-dir-local-variables-non-file-buffer))

(defun ai-project-vterm-in-dir (dir tool &optional command extra-env)
  "Open a vterm in DIR for TOOL.

If TOOL is `shell', open a plain interactive shell.
Otherwise open a shell and send COMMAND into it.

EXTRA-ENV is prepended to `process-environment' for the spawned shell."
  (unless (fboundp 'vterm)
    (error "vterm is not installed"))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (shell (ai-project--resolve-shell-command))
         (command (or command (ai-project--tool-command-for-dir tool dir)))
         (extra-env (or extra-env (ai-project--tool-env-for-dir tool dir)))
         (bufname (ai-project--buffer-name-for-dir tool dir))
         (signature (ai-project--launch-signature tool dir command extra-env))
         (existing (get-buffer bufname)))
    (cond
     ((and ai-project-reuse-live-buffers
           existing
           (ai-project--matching-buffer-p existing signature))
      (pop-to-buffer existing))
     (t
      (when existing
        (ai-project--kill-buffer-if-live existing))
      (let ((default-directory dir)
            (process-environment (append extra-env process-environment))
            (vterm-shell shell))
        (vterm bufname))
      (with-current-buffer bufname
        (ai-project--apply-dir-locals-for-buffer dir)
        (setq-local ai-project--signature signature)
        (setq-local ai-project--tool tool)
        (setq-local ai-project--project-root dir)
        (setq-local ai-project--command command))
      (unless (eq tool 'shell)
        (with-current-buffer bufname
          (when (and (stringp command) (not (string-empty-p command)))
            (vterm-send-string command)
            (vterm-send-return))))
      (pop-to-buffer bufname)))))

(defun ai-project-vterm-in-project (tool &optional command extra-env)
  "Open a vterm for TOOL in the current project root."
  (ai-project-vterm-in-dir (ai-project-root) tool command extra-env))

(defun ai-project-bootstrap-shell (dir)
  "Open a shell exactly in DIR, even if DIR is not an Emacs project root."
  (interactive "DDirectory: ")
  (ai-project-vterm-in-dir dir 'shell nil (ai-project-shell-env dir)))

(defun ai-project-shell ()
  "Launch a plain shell in the current project."
  (interactive)
  (let ((dir (ai-project-root)))
    (ai-project-vterm-in-dir dir 'shell nil (ai-project-shell-env dir))))

(defun ai-project-claude ()
  "Launch Claude Code in the current project."
  (interactive)
  (let ((dir (ai-project-root)))
    (ai-project-vterm-in-dir
     dir
     'claude
     (ai-project--tool-command-for-dir 'claude dir)
     (ai-project-claude-env dir))))

(defun ai-project-codex ()
  "Launch Codex in the current project."
  (interactive)
  (let ((dir (ai-project-root)))
    (ai-project-vterm-in-dir
     dir
     'codex
     (ai-project--tool-command-for-dir 'codex dir)
     (ai-project-codex-env dir))))

(defun ai-project-ant ()
  "Launch Anthropic ant in the current project."
  (interactive)
  (let ((dir (ai-project-root)))
    (ai-project-vterm-in-dir
     dir
     'ant
     (ai-project--tool-command-for-dir 'ant dir)
     (ai-project-ant-env dir))))

(defun ai-project-launch (tool)
  "Dispatch to one of the configured AI project tools."
  (interactive
   (list
    (intern
     (completing-read
      "Launch tool: "
      (mapcar #'symbol-name ai-project-dispatcher-tools)
      nil t))))
  (pcase tool
    ('shell  (ai-project-shell))
    ('claude (ai-project-claude))
    ('codex  (ai-project-codex))
    ('ant    (ai-project-ant))
    (_ (error "Unknown ai-project tool: %S" tool))))

(defun ai-project-files-sorted-by-name (project)
  "Return PROJECT files sorted by path/name."
  (sort (copy-sequence (project-files project)) #'string<))

(defun ai-project-find-file-by-name ()
  "Like `project-find-file', but candidates are sorted by name."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (files (ai-project-files-sorted-by-name pr))
         (choice (completing-read
                  "Find file (project, name-sorted): "
                  files nil t)))
    (find-file (expand-file-name choice root))))

(defun ai-project-remember-current ()
  "Remember current project, if any."
  (when-let ((pr (project-current nil)))
    (project-remember-project pr)))

(defun ai-project--enable-project-remembering ()
  "Enable remembering visited projects."
  (add-hook 'find-file-hook #'ai-project-remember-current))

(defun ai-project--disable-project-remembering ()
  "Disable remembering visited projects."
  (remove-hook 'find-file-hook #'ai-project-remember-current))

(defun ai-project--enable-mode-line ()
  "Enable project.el mode line integration."
  (setq project-mode-line t)
  (unless (member '(project-mode-line project-mode-line-format) mode-line-format)
    (setq-default mode-line-format
                  (append mode-line-format
                          '((project-mode-line project-mode-line-format)))))
  (with-eval-after-load 'project
    (setq project-mode-line-format
          '(:eval
            (when-let* ((pr (project-current)))
              (propertize
               (format " [PROJ:%s]" (project-name pr))
               'face 'mode-line-emphasis
               'help-echo "mouse-1: Project menu"
               'mouse-face 'mode-line-highlight
               'local-map project-mode-line-map))))))

(defun ai-project--default-switch-commands ()
  "Return default `project-switch-commands' entries for ai-project."
  '((project-find-file "Find file")
    (ai-project-find-file-by-name "Sorted find file")
    (project-find-dir "Find dir")
    (project-dired "Dired")
    (project-search "iGrep")
    (project-find-regexp "Grep")
    (ai-project-shell "Shell")
    (ai-project-ant "Ant")
    (ai-project-claude "Claude")
    (ai-project-codex "Codex")
    (ai-project-launch "AI launcher")))

(defun ai-project-install-default-switch-commands ()
  "Install a useful default value for `project-switch-commands'."
  (interactive)
  (setq project-switch-commands (ai-project--default-switch-commands)))

(defvar ai-project-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `ai-project-mode'.")

;;;###autoload
(define-minor-mode ai-project-mode
  "Global mode for AI-oriented project.el helpers."
  :global t
  :group 'ai-project
  :keymap ai-project-mode-map
  (setq project-list-file ai-project-project-list-file)
  (if ai-project-mode
      (progn
        (when ai-project-enable-project-remembering
          (ai-project--enable-project-remembering))
        (when ai-project-enable-mode-line
          (ai-project--enable-mode-line))
        (define-key project-prefix-map (kbd "t") #'ai-project-shell)
        (define-key project-prefix-map (kbd "A") #'ai-project-ant)
        (define-key project-prefix-map (kbd "C") #'ai-project-claude)
        (define-key project-prefix-map (kbd "X") #'ai-project-codex)
        (define-key project-prefix-map (kbd "L") #'ai-project-launch)
        (define-key project-prefix-map (kbd "F") #'ai-project-find-file-by-name))
    (ai-project--disable-project-remembering)))

(provide 'ai-project)

;;; ai-project.el ends here
