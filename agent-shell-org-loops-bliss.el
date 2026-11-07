;;; agent-shell-org-loops-bliss.el --- Run org loops headings with bliss -*- lexical-binding: t; -*-

;; Author: noonker
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: convenience, org, tools

;;; Commentary:
;;
;; A runner for `agent-shell-org-loops': a heading whose runner tag has
;; `:bliss TARGET' runs as `bliss ai' on that target instead of in a local
;; agent-shell.  Its output becomes the reply once the workload finishes.
;;
;; bliss remembers nothing between runs, so each turn sends the whole
;; subtree, and nobody can answer permission prompts, so the preamble says
;; so.  The core loads this file on demand; nothing here needs requiring.
;;
;; It talks to bliss only through `--json' output:
;;   bliss ai --json ...     -> {"name", "target"}
;;   bliss status --json N   -> {"state", "finished", "exit", ...}

;;; Code:

(require 'agent-shell-org-loops)
(require 'ansi-color)
(require 'json)

(declare-function bliss-attach "bliss")
(declare-function bliss-logs "bliss")

(defcustom agent-shell-org-loops-bliss-command '("bliss")
  "Command, as a list, that runs bliss.  It needs `--json' support."
  :type '(repeat string)
  :group 'agent-shell-org-loops)

(defcustom agent-shell-org-loops-bliss-poll-interval 15
  "Seconds between checks on a running bliss workload."
  :type 'number
  :group 'agent-shell-org-loops)

(defcustom agent-shell-org-loops-bliss-preamble
  "You are working on a task taken from an org-mode outline.
You are running non-interactively: nobody can answer questions or approve
tool permissions while you work.  If a tool you need is refused, do not
retry it; say what you would have run and why, and stop.
When you finish, end your final message with a single line of
one of the following forms so the org buffer state can be updated:
  STATE: DONE       -- the task is complete
  STATE: NEEDINFO   -- you need clarification, or were refused a permission
To propose subtasks the user may want to run separately, include one
or more blocks of the following form anywhere in your reply:
  #+begin_src loops-task
  <one-line title>
  <optional body lines describing what should be done>
  #+end_src
Each block becomes a TODO child heading under your reply; the user
decides whether to run it.  Do not use this for work you are performing
yourself in this run."
  "Preamble for headings run through bliss, which can't take permission answers."
  :type 'string
  :group 'agent-shell-org-loops)

(defcustom agent-shell-org-loops-bliss-stop-when-done 'in-place
  "Whether to run `bliss stop' once a workload's reply is written back.
bliss keeps a session open after the command, and its env.sh (your
environment, secrets included) until `bliss stop'.  The reply already
holds the output, so stopping loses only the session.
  in-place -- stop on targets that work in place (`here'), where stop
              leaves your directory alone
  t        -- stop on every target.  On a container or remote target
              this deletes the workload's files, including edits the
              agent made that you haven't pulled.
  nil      -- never; stop them yourself"
  :type '(choice (const :tag "Only in-place targets" in-place)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'agent-shell-org-loops)

(defvar agent-shell-org-loops-bliss--turns nil
  "In-flight bliss turns, as plists with :uuid :origin :name :link :started.")

;;;; Talking to bliss

(defun agent-shell-org-loops-bliss--args (args)
  "Return the full command line for bliss ARGS."
  (append agent-shell-org-loops-bliss-command args))

(defun agent-shell-org-loops-bliss--async (args callback)
  "Run bliss with ARGS in the background.
Call CALLBACK with the exit status, stdout and stderr, once it has ended."
  (let ((out (generate-new-buffer " *loops bliss*"))
        (err (generate-new-buffer " *loops bliss stderr*")))
    (make-process
     :name "agent-shell-org-loops-bliss"
     :buffer out
     :stderr err
     :command (agent-shell-org-loops-bliss--args args)
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         ;; The stderr pipe can lag behind the main process; let it drain.
         (when-let* ((errproc (get-buffer-process err)))
           (while (accept-process-output errproc 0.05)))
         (let ((stdout (with-current-buffer out (buffer-string)))
               (stderr (with-current-buffer err (buffer-string))))
           (kill-buffer out)
           (kill-buffer err)
           (funcall callback (process-exit-status proc)
                    (ansi-color-filter-apply stdout)
                    (ansi-color-filter-apply stderr))))))))

(defun agent-shell-org-loops-bliss--json (text)
  "Parse the last line of TEXT as a JSON object, or return nil."
  (let ((line (car (last (split-string (string-trim text) "\n")))))
    (ignore-errors
      (json-parse-string line :object-type 'plist :null-object nil
                         :false-object nil))))

(defun agent-shell-org-loops-bliss--last-line (text)
  "Return the last non-empty line of TEXT, for error messages."
  (car (last (split-string (string-trim text) "\n" t))))

(defun agent-shell-org-loops-bliss--status-now (name)
  "Return the status plist of workload NAME, waiting for it, or nil."
  (with-temp-buffer
    (when (zerop (apply #'call-process
                        (car agent-shell-org-loops-bliss-command) nil
                        (list t nil) nil
                        (append (cdr agent-shell-org-loops-bliss-command)
                                (list "status" "--json" name))))
      (agent-shell-org-loops-bliss--json (buffer-string)))))

;;;; Links

(defun agent-shell-org-loops-bliss--link (name)
  "Return an org link to the bliss workload NAME."
  (org-link-make-string (concat "bliss:" name) (concat "bliss " name)))

;;;###autoload
(defun agent-shell-org-loops-bliss-follow-link (name _arg)
  "Attach to the bliss workload NAME, or show its logs once it has finished.
Uses emacs-bliss when it is installed."
  (let* ((status (agent-shell-org-loops-bliss--status-now name))
         (running (and status (not (plist-get status :finished)))))
    (cond
     ((null status)
      (user-error "bliss workload %s is gone (stopped once its reply came back); \
its output is in the Reply" name))
     ((and running (fboundp 'bliss-attach)) (bliss-attach name))
     ((fboundp 'bliss-logs) (bliss-logs name))
     (t
      (with-current-buffer (get-buffer-create (format "*bliss logs %s*" name))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (apply #'call-process (car agent-shell-org-loops-bliss-command) nil t nil
                 (append (cdr agent-shell-org-loops-bliss-command)
                         (list "logs" name)))
          (ansi-color-filter-region (point-min) (point-max)))
        (special-mode)
        (pop-to-buffer (current-buffer)))))))

;;;; Starting a turn

(defun agent-shell-org-loops-bliss--edits-ok-p (runner)
  "Return non-nil unless the heading at point will likely fail on permissions.
That is an approach with :edits on a RUNNER without :unattended, where
nobody can approve the edits.  Then ask whether to run it anyway."
  (let ((approach (assoc-default
                   (seq-find (lambda (tag)
                               (assoc tag agent-shell-org-loops-approaches))
                             (org-get-tags))
                   agent-shell-org-loops-approaches)))
    (or (plist-get runner :unattended)
        (not (plist-get approach :edits))
        (y-or-n-p (format "This approach edits files, but bliss on %s can't \
approve edits without :unattended.  Run anyway? "
                          (plist-get runner :bliss))))))

;;;###autoload
(defun agent-shell-org-loops-bliss-trigger (runner)
  "Start the heading at point as `bliss ai' on RUNNER's target.
Called by the core for a runner with `:bliss'."
  (let ((dir (file-name-as-directory
              (expand-file-name (agent-shell-org-loops--resolve-project)))))
    (cond
     ((file-remote-p dir)
      (user-error "bliss runs on this machine; %s is remote" dir))
     ((not (executable-find (car agent-shell-org-loops-bliss-command)))
      (user-error "bliss is not installed; see `agent-shell-org-loops-bliss-command'"))
     ((not (agent-shell-org-loops-bliss--edits-ok-p runner))
      (agent-shell-org-loops--set-todo-silently "TODO")
      (message "agent-shell-org-loops: not started; set to TODO"))
     (t (agent-shell-org-loops-bliss--start runner dir)))))

(defun agent-shell-org-loops-bliss--start (runner dir)
  "Launch the heading at point on RUNNER from directory DIR."
  (let* ((origin-mk (point-marker))
         (uuid (agent-shell-org-loops--uuid))
         (built (agent-shell-org-loops--build-prompt
                 t agent-shell-org-loops-bliss-preamble))
         (args (append (list "--target" (plist-get runner :bliss) "ai" "--json")
                       (when-let* ((agent (plist-get runner :agent)))
                         (list "--agent" agent))
                       (when (plist-get runner :unattended) (list "--unattended"))
                       (list "--" (car built))))
         (turn (list :uuid uuid :origin origin-mk :started (float-time)
                     :sent-markers (cdr built)))
         (default-directory dir))
    (org-entry-put (point) "AGENT_PENDING" uuid)
    (agent-shell-org-loops--insert-placeholder uuid)
    (agent-shell-org-loops--set-todo-silently
     agent-shell-org-loops-inprogress-state)
    (agent-shell-org-loops-bliss--async
     args
     (lambda (status stdout stderr)
       (let ((name (and (zerop status)
                        (plist-get (agent-shell-org-loops-bliss--json stdout) :name))))
         (cond
          ((null name)
           (agent-shell-org-loops--abandon-placeholder
            uuid (format "bliss failed: %s"
                         (or (agent-shell-org-loops-bliss--last-line stderr)
                             (format "exit %s" status))))
           (agent-shell-org-loops--at-origin origin-mk
             (org-entry-delete (point) "AGENT_PENDING")
             (agent-shell-org-loops--set-todo-silently "NEEDINFO")))
          ;; Cancelled while bliss was still starting it.
          ((not (equal uuid (agent-shell-org-loops--at-origin origin-mk
                              (org-entry-get (point) "AGENT_PENDING"))))
           (agent-shell-org-loops-bliss--async (list "stop" name) #'ignore))
          (t (agent-shell-org-loops-bliss--started turn name))))))))

(defun agent-shell-org-loops-bliss--started (turn name)
  "Record that TURN runs as bliss workload NAME, and start watching it."
  (let ((link (agent-shell-org-loops-bliss--link name)))
    (setq turn (plist-put (plist-put turn :name name) :link link))
    (agent-shell-org-loops--at-origin (plist-get turn :origin)
      (org-entry-put (point) "BLISS_WORKLOAD" name))
    (when-let* ((loc (agent-shell-org-loops--find-placeholder
                      (plist-get turn :uuid))))
      (with-current-buffer (car loc)
        (save-excursion
          (goto-char (cdr loc))
          (end-of-line)
          (insert " " link))))
    (push turn agent-shell-org-loops-bliss--turns)
    (agent-shell-org-loops-bliss--poll turn)))

;;;; Watching a turn

(defun agent-shell-org-loops-bliss--poll (turn)
  "Check TURN's workload in a while; write the reply back once it has finished.
A workload bliss no longer knows counts as finished."
  (run-at-time
   agent-shell-org-loops-bliss-poll-interval nil
   (lambda ()
     (when (memq turn agent-shell-org-loops-bliss--turns)
       (agent-shell-org-loops-bliss--async
        (list "status" "--json" (plist-get turn :name))
        (lambda (status stdout _stderr)
          (let ((info (and (zerop status)
                           (agent-shell-org-loops-bliss--json stdout))))
            (if (and info (not (plist-get info :finished)))
                (agent-shell-org-loops-bliss--poll turn)
              (agent-shell-org-loops-bliss--finish turn)))))))))

(defun agent-shell-org-loops-bliss--finish (turn)
  "Fetch TURN's logs and write them back as the reply."
  (agent-shell-org-loops-bliss--async
   (list "logs" (plist-get turn :name))
   (lambda (_status stdout _stderr)
     (when (memq turn agent-shell-org-loops-bliss--turns)
       (setq agent-shell-org-loops-bliss--turns
             (delq turn agent-shell-org-loops-bliss--turns))
       (agent-shell-org-loops--at-origin (plist-get turn :origin)
         (org-entry-delete (point) "BLISS_WORKLOAD"))
       ;; The output may end with bliss's own lines after the agent's.
       (agent-shell-org-loops--complete-turn turn stdout t)
       (agent-shell-org-loops-bliss--maybe-stop (plist-get turn :name))))))

(defun agent-shell-org-loops-bliss--maybe-stop (name)
  "Stop workload NAME if `agent-shell-org-loops-bliss-stop-when-done' says to."
  (pcase agent-shell-org-loops-bliss-stop-when-done
    ('nil nil)
    ('in-place
     (agent-shell-org-loops-bliss--async
      (list "list" "--json")
      (lambda (status stdout _stderr)
        (when-let* (((zerop status))
                    (rows (ignore-errors
                            (json-parse-string stdout :object-type 'plist
                                               :array-type 'list
                                               :false-object nil)))
                    (row (seq-find (lambda (r) (equal (plist-get r :name) name))
                                   rows))
                    ((plist-get row :in_place)))
          (agent-shell-org-loops-bliss--async (list "stop" name) #'ignore)))))
    (_ (agent-shell-org-loops-bliss--async (list "stop" name) #'ignore))))

(defun agent-shell-org-loops-bliss--turn (uuid)
  "Return the in-flight bliss turn for UUID, or nil."
  (seq-find (lambda (turn) (equal (plist-get turn :uuid) uuid))
            agent-shell-org-loops-bliss--turns))

;;;; Hooks the core calls

;;;###autoload
(defun agent-shell-org-loops-bliss-cancel (uuid)
  "Stop the bliss workload running turn UUID for the heading at point."
  (let ((turn (agent-shell-org-loops-bliss--turn uuid))
        (name (org-entry-get (point) "BLISS_WORKLOAD")))
    (when turn
      (setq agent-shell-org-loops-bliss--turns
            (delq turn agent-shell-org-loops-bliss--turns))
      (setq name (plist-get turn :name)))
    ;; `bliss stop' ends the session; on `here' it leaves the directory alone.
    (when name
      (agent-shell-org-loops-bliss--async (list "stop" name) #'ignore))
    (org-entry-delete (point) "BLISS_WORKLOAD")))

;;;###autoload
(defun agent-shell-org-loops-bliss-resume (uuid workload)
  "Watch WORKLOAD again for turn UUID of the heading at point.
A bliss workload outlives Emacs, so a reopened file picks it back up."
  (unless (agent-shell-org-loops-bliss--turn uuid)
    (let ((turn (list :uuid uuid :origin (point-marker) :name workload
                      :started (float-time)
                      :link (agent-shell-org-loops-bliss--link workload))))
      (push turn agent-shell-org-loops-bliss--turns)
      (agent-shell-org-loops-bliss--poll turn))))

(defun agent-shell-org-loops-bliss-list-rows ()
  "Return dashboard rows for in-flight bliss turns."
  (mapcar (lambda (turn)
            (let ((mk (plist-get turn :origin)))
              (list (list :turn nil turn)
                    (vector "bliss"
                            (or (agent-shell-org-loops--marker-heading mk) "(gone)")
                            (agent-shell-org-loops--marker-file mk)
                            (agent-shell-org-loops--format-elapsed
                             (plist-get turn :started))
                            (or (plist-get turn :name) "")))))
          agent-shell-org-loops-bliss--turns))

(provide 'agent-shell-org-loops-bliss)
;;; agent-shell-org-loops-bliss.el ends here
