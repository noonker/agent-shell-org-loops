;;; agent-shell-org-loops.el --- Drive agent-shell from org-mode TODO transitions -*- lexical-binding: t; -*-

;; Author: noonker
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6") (agent-shell "0.1"))
;; Keywords: convenience, org, tools

;;; Commentary:
;;
;; In any org file that starts with `#+LOOPS: true', transitioning a heading
;; to the READY todo keyword launches (or reuses) an agent-shell session with
;; the heading subtree as context.  Agent replies are written back as child
;; headings.  Tool permission requests are surfaced as inline
;; `agent-shell-permission' babel blocks that the user can approve with
;; `C-c C-c'.
;;
;; State machine expected on the org side:
;;   TODO       -- authored but not yet handed to the agent
;;   READY      -- picked up by the agent (triggers a turn)
;;   INPROGRESS -- turn is in flight (set automatically after READY fires)
;;   PERMISSION -- agent is blocked awaiting a permission decision
;;   NEEDINFO   -- agent finished a turn asking for clarification
;;   DONE       -- agent finished a turn and considers work complete
;;   CANCELLED  -- user aborted; running turn (if any) gets interrupted
;;
;; Follow-up prompts are built from the origin body + any direct-child
;; subtrees whose heading does NOT carry the `:done:' tag (customizable
;; via `agent-shell-org-loops-sent-tag').  Once a turn completes, the
;; origin and every sent child get tagged; Reply and Permission-request
;; headings are tagged at creation.  To send a follow-up, add a child
;; heading with whatever content you like — it'll be picked up on the
;; next READY transition and tagged after the turn completes.
;;
;; File-level keywords honored:
;;   #+LOOPS: true              -- enable this file
;;   #+LOOPS_PROJECT: /some/dir -- default `default-directory' for shells
;;   #+LOOPS_AGENT: claude-code -- which agent-shell config to spawn (default)
;;   #+LOOPS_SHARED_SHELL: t    -- reuse a single shell for every heading in
;;                                 this file, instead of one per heading
;;
;; Heading-level properties honored (in the tracked drawer per heading):
;;   :AGENT_SHELL_BUFFER:  -- name of the shell buffer (reused if still alive)
;;   :AGENT_SHELL_SESSION: -- ACP session id (informational; not resumed)
;;   :LOOPS_PROJECT:       -- per-heading override of the working directory
;;   :LOOPS_AGENT:         -- per-heading override of the agent config
;;   :AGENT_PENDING:       -- uuid of the currently in-flight turn, if any
;;
;; Tag `:noloop:' opts a heading out entirely.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-capture)
(require 'ob)
(require 'cl-lib)
(require 'subr-x)
(require 'map)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--prompt-queue-enqueue "agent-shell-prompt-queue")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function shell-maker-busy "shell-maker")
(declare-function notifications-notify "notifications")
(declare-function alert "alert")
(defvar agent-shell-permission-responder-function)
(defvar agent-shell--state)
(defvar org-state) ; dynamically bound by `org-after-todo-state-change-hook'

;;;; Customization

(defgroup agent-shell-org-loops nil
  "Drive agent-shell sessions from org-mode TODO transitions."
  :group 'org
  :prefix "agent-shell-org-loops-")

(defcustom agent-shell-org-loops-trigger-state "READY"
  "Todo keyword that hands a heading off to the agent."
  :type 'string)

(defcustom agent-shell-org-loops-inprogress-state "INPROGRESS"
  "Todo keyword set on a heading while its turn is in flight.
Applied automatically after the trigger state fires, and re-applied
after a permission response resumes the turn."
  :type 'string)

(defcustom agent-shell-org-loops-sent-tag "done"
  "Org tag applied to headings whose content has been sent to the agent.
On every turn, only the origin body and direct-child subtrees lacking
this tag are included in the prompt.  Agent-generated Reply and
Permission-request children are tagged at creation so they aren't
re-fed.  The origin heading and any user-added children are tagged
after the turn completes successfully — so a cancelled or crashed turn
leaves their content available for the next attempt."
  :type 'string)

(defcustom agent-shell-org-loops-terminal-states
  '("DONE" "NEEDINFO" "PERMISSION" "CANCELLED")
  "Todo keywords the agent may drive the heading into."
  :type '(repeat string))

(defcustom agent-shell-org-loops-default-agent "claude-code"
  "Default agent config identifier used when a file/heading omits one."
  :type 'string)

(defcustom agent-shell-org-loops-agent-configs
  '(("claude-code" . agent-shell-anthropic-make-claude-code-config))
  "Alist mapping an agent id string to a zero-arg config constructor."
  :type '(alist :key-type string :value-type function))

(defcustom agent-shell-org-loops-notify-on-complete t
  "Whether to notify when a turn completes and the origin buffer is hidden.
Uses `notifications-notify' (dbus) when available, then `alert', then a
plain `message'.  Set to nil to disable."
  :type 'boolean)

(defcustom agent-shell-org-loops-system-preamble
  "You are collaborating inside an org-mode outline.
When you finish your turn, end your final message with a single line of
one of the following forms so the org buffer state can be updated:
  STATE: DONE       -- the task is complete
  STATE: NEEDINFO   -- you need clarification from the user
If you need to run a tool that requires permission, request it normally;
the user will receive a permission prompt inside their org buffer.
To propose subtasks the user may want to run separately, include one
or more blocks of the following form anywhere in your reply:
  #+begin_src loops-task
  <one-line title>
  <optional body lines describing what should be done>
  #+end_src
Each block becomes a TODO child heading under your reply; the user
decides whether to promote it to READY.  Do not use this for work you
are performing yourself in this turn."
  "Preamble prepended to the first prompt of every new shell."
  :type 'string)

;;;; Internal state

(defvar agent-shell-org-loops--shell-registry (make-hash-table :test 'eq)
  "Map from shell buffer -> plist of tracking info.
Plist keys:
  :origin-buffer   -- the org buffer that owns this shell
  :pending-queue   -- FIFO of pending turn plists, front = in-flight
  :permissions     -- hash-table uuid -> permission plist for open prompts")

(defvar agent-shell-org-loops--suppress-todo-hook nil
  "When non-nil, `agent-shell-org-loops--after-todo-state-change' returns early.
Let-bind while we programmatically drive `org-todo' from callbacks.")

(defvar-local agent-shell-org-loops--shared-shell nil
  "Cached shell buffer for `#+LOOPS_SHARED_SHELL: t' files.")

;;;; Small utilities

(defun agent-shell-org-loops--uuid ()
  "Return a short random slug suitable for use as a placeholder id."
  (let ((s (md5 (format "%s-%s-%s-%s"
                        (emacs-pid) (float-time) (random most-positive-fixnum)
                        (or (buffer-file-name) (buffer-name))))))
    (format "%s-%s-%s"
            (substring s 0 8) (substring s 8 12) (substring s 12 20))))

(defun agent-shell-org-loops--file-keyword (key)
  "Return value of `#+KEY:' in current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (re (format "^#\\+%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key))))
      (when (re-search-forward re nil t)
        (let ((v (match-string-no-properties 1)))
          (and v (not (string-empty-p v)) v))))))

(defun agent-shell-org-loops--truthy-p (v)
  "Return non-nil when V is a truthy keyword value string."
  (and v (member (downcase v) '("t" "true" "yes" "on"))))

(defun agent-shell-org-loops--loops-enabled-p (&optional buffer)
  "Return non-nil when BUFFER (or current) has `#+LOOPS: true'."
  (with-current-buffer (or buffer (current-buffer))
    (and (derived-mode-p 'org-mode)
         (agent-shell-org-loops--truthy-p
          (agent-shell-org-loops--file-keyword "LOOPS")))))

(defun agent-shell-org-loops--shared-shell-p ()
  "Return non-nil when the current buffer opts into a single shared shell."
  (agent-shell-org-loops--truthy-p
   (agent-shell-org-loops--file-keyword "LOOPS_SHARED_SHELL")))

(defun agent-shell-org-loops--loops-buffers ()
  "Return the list of live buffers with LOOPS enabled."
  (seq-filter #'agent-shell-org-loops--loops-enabled-p (buffer-list)))

(defun agent-shell-org-loops--resolve-project ()
  "Return the working directory for the heading at point.
Falls back to the file-level `#+LOOPS_PROJECT:', then `default-directory'."
  (or (org-entry-get (point) "LOOPS_PROJECT" t)
      (agent-shell-org-loops--file-keyword "LOOPS_PROJECT")
      default-directory))

(defun agent-shell-org-loops--resolve-agent ()
  "Return the agent id string used to spawn a shell for the heading at point."
  (or (org-entry-get (point) "LOOPS_AGENT" t)
      (agent-shell-org-loops--file-keyword "LOOPS_AGENT")
      agent-shell-org-loops-default-agent))

(defun agent-shell-org-loops--agent-config (agent-id)
  "Build and return the agent-shell config for AGENT-ID."
  (let ((ctor (cdr (assoc agent-id agent-shell-org-loops-agent-configs))))
    (unless (and ctor (fboundp ctor))
      (error "Unknown agent id '%s'; see `agent-shell-org-loops-agent-configs'"
             agent-id))
    (funcall ctor)))

;;;; Sent-tag bookkeeping

(defun agent-shell-org-loops--heading-sent-p ()
  "Return non-nil when the heading at point carries the sent tag."
  (member agent-shell-org-loops-sent-tag (org-get-tags nil t)))

(defun agent-shell-org-loops--add-sent-tag ()
  "Add the sent tag to the heading at point (idempotent)."
  (org-toggle-tag agent-shell-org-loops-sent-tag 'on))

(defun agent-shell-org-loops--tag-marker-sent (mk)
  "Add the sent tag to the heading at marker MK."
  (when (and (markerp mk) (buffer-live-p (marker-buffer mk)))
    (with-current-buffer (marker-buffer mk)
      (save-excursion
        (goto-char mk)
        (when (ignore-errors (org-back-to-heading t))
          (agent-shell-org-loops--add-sent-tag))))))

;;;; Context assembly

(defun agent-shell-org-loops--ancestor-crumbs ()
  "Return a string of ancestor headings above point, one per line."
  (let (crumbs)
    (save-excursion
      (while (org-up-heading-safe)
        (push (concat (make-string (org-current-level) ?*)
                      " "
                      (org-get-heading t t t t))
              crumbs)))
    (if crumbs
        (concat "Ancestors:\n" (mapconcat #'identity crumbs "\n") "\n\n")
      "")))

(defun agent-shell-org-loops--strip-drawer (raw)
  "Return RAW with a leading PROPERTIES drawer removed."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*\n" nil t)
      (let ((db (match-beginning 0)))
        (when (re-search-forward "^[ \t]*:END:[ \t]*\n?" nil t)
          (delete-region db (point)))))
    (buffer-string)))

(defun agent-shell-org-loops--origin-body ()
  "Return just the origin heading + body before its first child."
  (save-excursion
    (org-back-to-heading t)
    (let* ((beg (point))
           (end (save-excursion
                  (if (org-goto-first-child)
                      (point)
                    (org-end-of-subtree t t)
                    (point)))))
      (agent-shell-org-loops--strip-drawer
       (buffer-substring-no-properties beg end)))))

(defun agent-shell-org-loops--gather-untagged ()
  "Return (PARTS . MARKERS): untagged origin body + direct-child subtrees.
PARTS is a list of body strings ready to concatenate.  MARKERS is a
list of heading markers (origin plus each included child) that should
receive the sent tag once the turn completes successfully."
  (save-excursion
    (org-back-to-heading t)
    (let (parts to-tag)
      (unless (agent-shell-org-loops--heading-sent-p)
        (push (point-marker) to-tag)
        (push (agent-shell-org-loops--origin-body) parts))
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (when (org-goto-first-child)
          (while (< (point) end)
            (unless (agent-shell-org-loops--heading-sent-p)
              (push (point-marker) to-tag)
              (let* ((sec-beg (point))
                     (sec-end (save-excursion
                                (org-end-of-subtree t t) (point))))
                (push (agent-shell-org-loops--strip-drawer
                       (buffer-substring-no-properties sec-beg sec-end))
                      parts)))
            (unless (org-goto-sibling) (goto-char end)))))
      (cons (nreverse parts) (nreverse to-tag)))))

(defun agent-shell-org-loops--build-prompt ()
  "Compose the prompt sent to the agent for the heading at point.
Returns (cons PROMPT MARKERS): PROMPT is the string to send; MARKERS is
the list of heading markers to tag once the turn completes.  The
preamble is included on the first turn (origin not yet tagged)."
  (let* ((first-turn-p (not (agent-shell-org-loops--heading-sent-p)))
         (tags (seq-remove
                (lambda (tag)
                  (equal tag agent-shell-org-loops-sent-tag))
                (org-get-tags nil t)))
         (heading (org-get-heading t t t t))
         (crumbs (agent-shell-org-loops--ancestor-crumbs))
         (gathered (agent-shell-org-loops--gather-untagged))
         (parts (car gathered))
         (markers (cdr gathered))
         (body (mapconcat #'identity parts "\n")))
    (cons
     (concat
      (when first-turn-p
        (concat agent-shell-org-loops-system-preamble "\n\n"))
      (format "Org file: %s\n" (or (buffer-file-name) (buffer-name)))
      (format "Heading: %s\n" heading)
      (when tags
        (format "Tags: %s\n" (mapconcat #'identity tags " ")))
      "\n"
      crumbs
      (if first-turn-p "Subtree:\n" "Follow-up:\n")
      body)
     markers)))

;;;; Placeholder markers in the org buffer

(defun agent-shell-org-loops--insert-placeholder (uuid)
  "Insert a pending reply child heading with UUID under the heading at point.
Leaves point unchanged.  Returns a marker to the placeholder start."
  (save-excursion
    (org-back-to-heading t)
    (let ((level (org-current-level))
          (child-start nil))
      (org-end-of-subtree t t)
      ;; End-of-subtree can leave us on the following heading; back up if so.
      (unless (bolp) (insert "\n"))
      (setq child-start (point-marker))
      (insert (make-string (1+ level) ?*)
              " Reply <<agent-shell-pending:" uuid ">>\n"
              ":AGENT_TURN:\n"
              ":ID: " uuid "\n"
              ":END:\n"
              "[agent-shell working... " uuid "]\n")
      child-start)))

(defun agent-shell-org-loops--find-placeholder (uuid)
  "Return (BUFFER . POSITION) of the placeholder for UUID, or nil.
Searches every LOOPS buffer; the heading may have been refiled."
  (catch 'found
    (dolist (buf (agent-shell-org-loops--loops-buffers))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "<<agent-shell-pending:" (regexp-quote uuid) ">>")
                 nil t)
            (throw 'found (cons buf (match-beginning 0)))))))
    nil))

(defun agent-shell-org-loops--replace-placeholder (uuid response)
  "Replace the placeholder for UUID with RESPONSE text.
Returns (cons ORIGIN-MK REPLY-MK) or nil if placeholder was gone."
  (when-let ((loc (agent-shell-org-loops--find-placeholder uuid)))
    (with-current-buffer (car loc)
      (save-excursion
        (goto-char (cdr loc))
        (org-back-to-heading t)
        (let* ((reply-start (point))
               (reply-end (save-excursion (org-end-of-subtree t t) (point)))
               (level (org-current-level))
               (stars (make-string level ?*))
               (ts (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (delete-region reply-start reply-end)
          (goto-char reply-start)
          (insert stars " Reply " ts "\n"
                  (string-trim response) "\n")
          (save-excursion
            (goto-char reply-start)
            (agent-shell-org-loops--add-sent-tag))
          (let ((reply-mk (copy-marker reply-start)))
            (goto-char reply-start)
            (when (org-up-heading-safe)
              (cons (point-marker) reply-mk))))))))

;;;; Shell session registry

(defun agent-shell-org-loops--tracking (shell-buffer)
  "Return the tracking plist for SHELL-BUFFER, or nil."
  (gethash shell-buffer agent-shell-org-loops--shell-registry))

(defun agent-shell-org-loops--update-tracking (shell-buffer key value)
  "Set KEY to VALUE in the tracking plist of SHELL-BUFFER."
  (let ((pl (agent-shell-org-loops--tracking shell-buffer)))
    (setf (plist-get pl key) value)
    (puthash shell-buffer pl agent-shell-org-loops--shell-registry)))

(defun agent-shell-org-loops--enqueue-turn (shell-buffer turn)
  "Append TURN (a plist) to SHELL-BUFFER's pending queue.
Stamps `:started' at enqueue time if the caller omitted it."
  (unless (plist-get turn :started)
    (setf (plist-get turn :started) (float-time)))
  (let* ((pl (agent-shell-org-loops--tracking shell-buffer))
         (q (plist-get pl :pending-queue)))
    (agent-shell-org-loops--update-tracking
     shell-buffer :pending-queue (append q (list turn)))))

(defun agent-shell-org-loops--pop-turn (shell-buffer)
  "Pop the front turn from SHELL-BUFFER's queue and return it."
  (let* ((pl (agent-shell-org-loops--tracking shell-buffer))
         (q (plist-get pl :pending-queue))
         (front (car q)))
    (agent-shell-org-loops--update-tracking
     shell-buffer :pending-queue (cdr q))
    front))

(defun agent-shell-org-loops--get-or-create-shell (origin-heading-marker)
  "Return a live shell buffer for the origin heading at ORIGIN-HEADING-MARKER.
Reuses the file-wide shared shell when `#+LOOPS_SHARED_SHELL: t' is set,
otherwise falls back to the per-heading :AGENT_SHELL_BUFFER: property."
  (with-current-buffer (marker-buffer origin-heading-marker)
    (save-excursion
      (goto-char origin-heading-marker)
      (let* ((shared-p (agent-shell-org-loops--shared-shell-p))
             (existing (cond
                        (shared-p
                         (and (buffer-live-p agent-shell-org-loops--shared-shell)
                              agent-shell-org-loops--shared-shell))
                        (t
                         (let ((bname (org-entry-get (point) "AGENT_SHELL_BUFFER")))
                           (and bname (get-buffer bname))))))
             (project (agent-shell-org-loops--resolve-project))
             (agent-id (agent-shell-org-loops--resolve-agent)))
        (if (and existing (buffer-live-p existing))
            (progn
              (unless shared-p
                (org-entry-put (point) "AGENT_SHELL_BUFFER" (buffer-name existing)))
              existing)
          (let* ((default-directory (or project default-directory))
                 (config (agent-shell-org-loops--agent-config agent-id))
                 (shell (agent-shell--start :config config
                                            :no-focus t
                                            :new-session t
                                            :session-strategy 'new)))
            (agent-shell-org-loops--register-shell shell (current-buffer))
            (if shared-p
                (setq agent-shell-org-loops--shared-shell shell)
              (org-entry-put (point) "AGENT_SHELL_BUFFER" (buffer-name shell)))
            (when-let ((sid (with-current-buffer shell
                              (ignore-errors
                                (map-nested-elt agent-shell--state
                                                '(:session :id))))))
              (unless shared-p
                (org-entry-put (point) "AGENT_SHELL_SESSION" sid)))
            shell))))))

(defun agent-shell-org-loops--register-shell (shell-buffer origin-buffer)
  "Install subscriptions and tracking for SHELL-BUFFER owned by ORIGIN-BUFFER."
  (puthash shell-buffer
           (list :origin-buffer origin-buffer
                 :pending-queue nil
                 :permissions (make-hash-table :test 'equal))
           agent-shell-org-loops--shell-registry)
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'turn-complete
   :on-event (lambda (_evt)
               (agent-shell-org-loops--on-turn-complete shell-buffer)))
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'agent-message-chunk
   :on-event (lambda (evt)
               (agent-shell-org-loops--on-message-chunk shell-buffer evt)))
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'permission-response
   :on-event (lambda (evt)
               (agent-shell-org-loops--on-permission-response shell-buffer evt)))
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event 'clean-up
   :on-event (lambda (_evt)
               (remhash shell-buffer
                        agent-shell-org-loops--shell-registry))))

;;;; Chunk accumulation

(defun agent-shell-org-loops--on-message-chunk (shell-buffer evt)
  "Accumulate streamed text on the in-flight turn for SHELL-BUFFER."
  (when-let* ((data (map-elt evt :data))
              (chunk (map-elt data :text-chunk))
              (pl (agent-shell-org-loops--tracking shell-buffer))
              (q (plist-get pl :pending-queue))
              (front (car q)))
    (setf (plist-get front :accum)
          (concat (or (plist-get front :accum) "") chunk))))

;;;; Turn completion

(defun agent-shell-org-loops--parse-task-blocks (text)
  "Extract `loops-task' src blocks from TEXT.
Returns (cons CLEANED-TEXT TASKS): CLEANED-TEXT has the blocks removed;
TASKS is a list of plists with :title and :body.  Within a block, the
first non-empty line is the title and the rest is the body."
  (let ((tasks nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward
                "^#\\+begin_src[ \t]+loops-task[^\n]*\n" nil t)
          (let ((open-start (match-beginning 0))
                (body-start (match-end 0)))
            (when (re-search-forward "^#\\+end_src[ \t]*$" nil t)
              (let* ((body-end (match-beginning 0))
                     (close-end (match-end 0))
                     (block-body (buffer-substring-no-properties
                                  body-start body-end))
                     (lines (split-string block-body "\n"))
                     (non-empty (seq-drop-while
                                 (lambda (l) (string-empty-p (string-trim l)))
                                 lines))
                     (title (string-trim
                             (or (car non-empty) "(untitled task)")))
                     (body (string-trim-right
                            (mapconcat #'identity (cdr non-empty) "\n"))))
                (push (list :title title :body body) tasks)
                (delete-region open-start
                               (min (point-max) (1+ close-end))))))))
      (cons (string-trim (buffer-string)) (nreverse tasks)))))

(defun agent-shell-org-loops--materialize-tasks (reply-mk tasks)
  "Insert each of TASKS as a child TODO heading under REPLY-MK."
  (when (and tasks (markerp reply-mk) (buffer-live-p (marker-buffer reply-mk)))
    (with-current-buffer (marker-buffer reply-mk)
      (save-excursion
        (goto-char reply-mk)
        (org-back-to-heading t)
        (let ((level (org-current-level)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (dolist (task tasks)
            (let ((title (plist-get task :title))
                  (body (plist-get task :body)))
              (insert (make-string (1+ level) ?*) " TODO " title "\n")
              (unless (string-empty-p body)
                (insert body "\n")))))))))

(defun agent-shell-org-loops--on-turn-complete (shell-buffer)
  "Handle turn-complete for SHELL-BUFFER: write reply back and update state."
  (when-let* ((turn (agent-shell-org-loops--pop-turn shell-buffer))
              (uuid (plist-get turn :uuid)))
    (let* ((raw (or (plist-get turn :accum) ""))
           (state-split (agent-shell-org-loops--strip-state-marker raw))
           (parsed-state (car state-split))
           (task-split (agent-shell-org-loops--parse-task-blocks
                        (cdr state-split)))
           (tasks (cdr task-split))
           (body (car task-split))
           (body (cond
                  ((not (string-empty-p (string-trim body)))
                   (agent-shell-org-loops--markdown-to-org body))
                  (tasks
                   (format "(spawned %d subtask%s)"
                           (length tasks)
                           (if (= 1 (length tasks)) "" "s")))
                  (t
                   "(no message body — agent may have only run tools)")))
           (markers (agent-shell-org-loops--replace-placeholder uuid body))
           (origin-mk (car markers))
           (reply-mk (cdr markers)))
      (dolist (mk (plist-get turn :sent-markers))
        (agent-shell-org-loops--tag-marker-sent mk))
      (when reply-mk
        (agent-shell-org-loops--materialize-tasks reply-mk tasks))
      (cond
       ((null origin-mk)
        (message "agent-shell-org-loops: reply for %s dropped (origin buffer gone)"
                 uuid))
       ((not (buffer-live-p (marker-buffer origin-mk)))
        (message "agent-shell-org-loops: reply for %s written but origin dead"
                 uuid))
       (t
        (with-current-buffer (marker-buffer origin-mk)
          (save-excursion
            (goto-char origin-mk)
            (org-entry-delete (point) "AGENT_PENDING")
            (agent-shell-org-loops--set-todo-silently
             (or parsed-state "NEEDINFO"))
            (agent-shell-org-loops--maybe-notify
             (org-get-heading t t t t) (current-buffer)))))))))

(defun agent-shell-org-loops--markdown-to-org (body)
  "Rewrite ```lang ... ``` fenced blocks in BODY as `#+begin_src' blocks."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (while (re-search-forward "^```[ \t]*\\([^\n]*\\)$" nil t)
      (let* ((lang (string-trim (match-string 1)))
             (open-start (match-beginning 0))
             (open-end (match-end 0)))
        (when (re-search-forward "^```[ \t]*$" nil t)
          (let ((close-start (match-beginning 0))
                (close-end (match-end 0)))
            (goto-char close-start)
            (delete-region close-start close-end)
            (insert "#+end_src")
            (goto-char open-start)
            (delete-region open-start open-end)
            (insert (format "#+begin_src %s" lang))))))
    (buffer-string)))

(defun agent-shell-org-loops--maybe-notify (heading origin-buffer)
  "Fire a desktop notification for HEADING if ORIGIN-BUFFER isn't visible."
  (when (and agent-shell-org-loops-notify-on-complete
             (not (get-buffer-window origin-buffer t)))
    (let ((body (format "%s — %s"
                        (buffer-name origin-buffer)
                        (or heading "(no heading)"))))
      (cond
       ((and (featurep 'dbus) (fboundp 'notifications-notify))
        (ignore-errors
          (notifications-notify :title "agent-shell-org-loops"
                                :body body)))
       ((fboundp 'alert)
        (alert body :title "agent-shell-org-loops"))
       (t
        (message "agent-shell-org-loops: %s" body))))))

(defun agent-shell-org-loops--strip-state-marker (text)
  "Return (STATE . BODY): STATE from a trailing `STATE: X' line, or nil."
  (let ((trimmed (string-trim-right (or text ""))))
    (if (string-match "\\(?:\\`\\|\n\\)STATE:[ \t]*\\([A-Z]+\\)[ \t]*\\'"
                      trimmed)
        (let ((state (match-string 1 trimmed))
              (body (substring trimmed 0 (match-beginning 0))))
          (cons (upcase state) (string-trim-right body)))
      (cons nil trimmed))))

(defvar org-inhibit-logging)

(defun agent-shell-org-loops--set-todo-silently (state)
  "Set the todo state of the heading at point to STATE.
Does not re-fire our own hook and does not prompt for a log note."
  (let ((agent-shell-org-loops--suppress-todo-hook t)
        (org-inhibit-logging t))
    (org-todo state)))

;;;; Todo state change hook

(defun agent-shell-org-loops--after-todo-state-change ()
  "Dispatch on transitions into the trigger or CANCELLED states."
  (unless agent-shell-org-loops--suppress-todo-hook
    (when (agent-shell-org-loops--loops-enabled-p)
      (let ((state (substring-no-properties (or org-state ""))))
        (cond
         ((and (equal state agent-shell-org-loops-trigger-state)
               (not (member "noloop" (org-get-tags nil t))))
          (agent-shell-org-loops--trigger-heading))
         ((equal state "CANCELLED")
          (agent-shell-org-loops--cancel-heading)))))))

(defun agent-shell-org-loops--trigger-heading ()
  "Fire off a turn for the heading at point.
Refuses to fire when the heading already has an in-flight turn (would
create an orphan placeholder).  When the shell is busy the prompt is
queued via `agent-shell--prompt-queue-enqueue' and auto-drains at turn
completion."
  (save-excursion
    (org-back-to-heading t)
    (if (org-entry-get (point) "AGENT_PENDING")
        (message
         "agent-shell-org-loops: heading already has an in-flight turn; \
cancel it first")
      (let* ((origin-mk (point-marker))
             (uuid (agent-shell-org-loops--uuid))
             (built (agent-shell-org-loops--build-prompt))
             (prompt (car built))
             (sent-markers (cdr built))
             (shell (agent-shell-org-loops--get-or-create-shell origin-mk)))
        (org-entry-put (point) "AGENT_PENDING" uuid)
        (agent-shell-org-loops--insert-placeholder uuid)
        (agent-shell-org-loops--enqueue-turn
         shell (list :uuid uuid :origin origin-mk :accum ""
                     :sent-markers sent-markers))
        (agent-shell-org-loops--send-or-queue shell prompt)
        (agent-shell-org-loops--set-todo-silently
         agent-shell-org-loops-inprogress-state)))))

(defun agent-shell-org-loops--send-or-queue (shell prompt)
  "Send PROMPT to SHELL, or enqueue it if SHELL is busy."
  (with-current-buffer shell
    (require 'agent-shell-prompt-queue)
    (if (and (fboundp 'shell-maker-busy) (shell-maker-busy))
        (agent-shell--prompt-queue-enqueue :prompt prompt)
      (agent-shell--insert-to-shell-buffer
       :shell-buffer shell :text prompt :submit t :no-focus t))))

(defun agent-shell-org-loops--cancel-heading ()
  "Interrupt the in-flight turn for the heading at point, if any.
Removes the ghost entry from the shell's pending queue and rewrites the
placeholder — `agent-shell-interrupt' fires the on-failure path rather
than `turn-complete', so our own queue would otherwise drift."
  (let* ((bname (org-entry-get (point) "AGENT_SHELL_BUFFER"))
         (shell (or (and bname (get-buffer bname))
                    agent-shell-org-loops--shared-shell))
         (uuid (org-entry-get (point) "AGENT_PENDING")))
    (when (and shell (buffer-live-p shell))
      (with-current-buffer shell
        (ignore-errors (agent-shell-interrupt t)))
      (when uuid
        (agent-shell-org-loops--remove-turn-from-queue shell uuid)))
    (when uuid
      (agent-shell-org-loops--abandon-placeholder uuid "cancelled"))
    (org-entry-delete (point) "AGENT_PENDING")))

(defun agent-shell-org-loops--remove-turn-from-queue (shell uuid)
  "Drop the turn identified by UUID from SHELL's pending queue."
  (when-let ((pl (agent-shell-org-loops--tracking shell)))
    (agent-shell-org-loops--update-tracking
     shell :pending-queue
     (seq-remove (lambda (turn) (equal (plist-get turn :uuid) uuid))
                 (plist-get pl :pending-queue)))))

;;;; Capture integration

(defun agent-shell-org-loops--after-capture-finalize ()
  "If the just-finalized capture landed as READY in a LOOPS buffer, trigger."
  (unless org-note-abort
    (when-let* ((pos (org-capture-get :position))
                (buf (and (markerp pos) (marker-buffer pos))))
      (with-current-buffer buf
        (when (agent-shell-org-loops--loops-enabled-p)
          (save-excursion
            (goto-char pos)
            (when (ignore-errors (org-back-to-heading t))
              (when (equal (org-get-todo-state)
                           agent-shell-org-loops-trigger-state)
                (agent-shell-org-loops--trigger-heading)))))))))

;;;; Permission plumbing

(defun agent-shell-org-loops--permission-responder (permission)
  "Route agent-shell PERMISSION requests into the owning org buffer.
Always returns nil so agent-shell's own dialog also renders: the user can
accept/deny in either place, and the `permission-response' event handler
reconciles the org side regardless of which path is used."
  (let* ((shell (current-buffer))
         (tracking (agent-shell-org-loops--tracking shell)))
    (when tracking
      (let* ((tool-call (map-elt permission :tool-call))
             (options (map-elt permission :options))
             (respond (map-elt permission :respond))
             (request-id (map-elt tool-call :permission-request-id))
             (title (or (map-elt tool-call :title) "tool call"))
             (uuid (agent-shell-org-loops--uuid))
             (turn (car (plist-get tracking :pending-queue)))
             (origin-mk (and turn (plist-get turn :origin))))
        (when (and origin-mk respond request-id)
          (puthash uuid (list :respond respond
                              :options options
                              :request-id request-id
                              :shell shell
                              :origin origin-mk
                              :title title)
                   (plist-get tracking :permissions))
          (agent-shell-org-loops--insert-permission-block
           origin-mk uuid title tool-call)
          (with-current-buffer (marker-buffer origin-mk)
            (save-excursion
              (goto-char origin-mk)
              (agent-shell-org-loops--set-todo-silently "PERMISSION"))))))
    nil))

(defun agent-shell-org-loops--insert-permission-block (origin-mk uuid title tool-call)
  "Insert an `agent-shell-permission' babel block under the ORIGIN-MK heading.
The block is inserted with `:results silent' so babel's own results insertion
doesn't collide with the `#+RESULTS:' line the response event handler writes."
  (with-current-buffer (marker-buffer origin-mk)
    (save-excursion
      (goto-char origin-mk)
      (org-back-to-heading t)
      (let ((level (org-current-level))
            perm-start)
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (setq perm-start (point))
        (insert (make-string (1+ level) ?*)
                " Permission request <<agent-shell-permission:" uuid ">>\n"
                (format "%s\n\n" title)
                (format "#+begin_src agent-shell-permission :id %s :results silent\n"
                        uuid)
                (or (agent-shell-org-loops--tool-call-body tool-call) "")
                "\n#+end_src\n"
                "# C-c C-c in the block to allow; \"deny\" to reject.\n")
        (save-excursion
          (goto-char perm-start)
          (agent-shell-org-loops--add-sent-tag))))))

(defun agent-shell-org-loops--tool-call-body (tool-call)
  "Best-effort extraction of a human-readable body for TOOL-CALL."
  (let ((kind (map-elt tool-call :kind))
        (title (map-elt tool-call :title))
        (diff (map-elt tool-call :diff)))
    (cond
     (diff (format "%s" diff))
     ((and kind title) (format "%s: %s" kind title))
     (title title)
     (t (format "%S" tool-call)))))

(defun agent-shell-org-loops--find-permission (uuid)
  "Return (SHELL . RECORD) for permission UUID across every tracked shell."
  (catch 'found
    (maphash
     (lambda (shell pl)
       (let ((rec (gethash uuid (plist-get pl :permissions))))
         (when rec (throw 'found (cons shell rec)))))
     agent-shell-org-loops--shell-registry)
    nil))

(defun agent-shell-org-loops--find-permission-by-request-id (shell request-id)
  "Return (UUID . RECORD) for REQUEST-ID in SHELL's tracking, or nil."
  (when-let ((tracking (agent-shell-org-loops--tracking shell)))
    (catch 'found
      (maphash
       (lambda (uuid rec)
         (when (equal (plist-get rec :request-id) request-id)
           (throw 'found (cons uuid rec))))
       (plist-get tracking :permissions))
      nil)))

(defun agent-shell-org-loops--respond-permission (uuid decision)
  "Send DECISION (\"allow\" or \"deny\") for permission UUID.
Actual org-side write-back happens in `--on-permission-response'."
  (when-let* ((hit (agent-shell-org-loops--find-permission uuid))
              (rec (cdr hit))
              (respond (plist-get rec :respond))
              (options (plist-get rec :options))
              (kind (if (equal decision "deny") "reject_once" "allow_once"))
              (choice (or (seq-find (lambda (o) (equal (map-elt o :kind) kind))
                                    options)
                          (car options))))
    (funcall respond (map-elt choice :option-id))))

(defun agent-shell-org-loops--option-decision-label (options option-id)
  "Return a human-facing decision label for OPTION-ID within OPTIONS."
  (let ((kind (map-elt (seq-find (lambda (o) (equal (map-elt o :option-id) option-id))
                                 options)
                       :kind)))
    (pcase kind
      ("allow_once" "allow")
      ("allow_always" "allow-always")
      ("reject_once" "deny")
      ("reject_always" "deny-always")
      (_ (or kind "responded")))))

(defun agent-shell-org-loops--on-permission-response (shell-buffer evt)
  "Reconcile the org side when a permission response happens for SHELL-BUFFER.
Handles both org-block acceptance and shell-buffer acceptance uniformly."
  (when-let* ((data (map-elt evt :data))
              (request-id (map-elt data :request-id))
              (hit (agent-shell-org-loops--find-permission-by-request-id
                    shell-buffer request-id))
              (uuid (car hit))
              (rec (cdr hit))
              (origin-mk (plist-get rec :origin)))
    (let ((decision (if (map-elt data :cancelled)
                        "cancelled"
                      (agent-shell-org-loops--option-decision-label
                       (plist-get rec :options)
                       (map-elt data :option-id)))))
      (remhash uuid (plist-get (agent-shell-org-loops--tracking shell-buffer)
                               :permissions))
      (agent-shell-org-loops--mark-permission-resolved origin-mk uuid decision)
      (when (buffer-live-p (marker-buffer origin-mk))
        (with-current-buffer (marker-buffer origin-mk)
          (save-excursion
            (goto-char origin-mk)
            (agent-shell-org-loops--set-todo-silently
             agent-shell-org-loops-inprogress-state)))))))

(defun agent-shell-org-loops--mark-permission-resolved (origin-mk uuid decision)
  "Append a RESULTS line to the permission block for UUID under ORIGIN-MK."
  (when (and origin-mk (buffer-live-p (marker-buffer origin-mk)))
    (with-current-buffer (marker-buffer origin-mk)
      (save-excursion
        (goto-char origin-mk)
        (when (re-search-forward
               (concat "<<agent-shell-permission:" (regexp-quote uuid) ">>")
               nil t)
          (when (re-search-forward "^#\\+end_src[ \t]*$" nil t)
            (forward-line 1)
            ;; If a RESULTS line for this uuid already exists, do nothing —
            ;; idempotent in case an event fires twice.
            (unless (looking-at (concat "^#\\+RESULTS: "
                                        (regexp-quote uuid) "$"))
              (insert (format "#+RESULTS: %s\n: %s at %s\n"
                              uuid decision
                              (format-time-string "[%Y-%m-%d %a %H:%M]"))))))))))

;;;###autoload
(defun org-babel-execute:agent-shell-permission (_body params)
  "Approve the pending permission whose :id matches PARAMS.
Returns nil so no auto-inserted result line collides with the `#+RESULTS:'
that `--on-permission-response' writes."
  (let ((uuid (or (cdr (assq :id params))
                  (user-error "agent-shell-permission block missing :id"))))
    (unless (agent-shell-org-loops--find-permission uuid)
      (user-error "No pending permission for id %s" uuid))
    (agent-shell-org-loops--respond-permission uuid "allow")
    nil))

;;;###autoload
(defun agent-shell-org-loops-deny-permission-at-point ()
  "Deny the agent-shell-permission block at point."
  (interactive)
  (let* ((el (org-element-context))
         (params (and (eq (org-element-type el) 'src-block)
                      (org-babel-parse-header-arguments
                       (org-element-property :parameters el))))
         (uuid (cdr (assq :id params))))
    (unless uuid
      (user-error "Point is not on an agent-shell-permission block with :id"))
    (agent-shell-org-loops--respond-permission uuid "deny")))

;;;; Crash recovery

(defun agent-shell-org-loops--abandon-placeholder (uuid reason)
  "Rewrite the placeholder for UUID to a REASON marker."
  (when-let ((loc (agent-shell-org-loops--find-placeholder uuid)))
    (with-current-buffer (car loc)
      (save-excursion
        (goto-char (cdr loc))
        (org-back-to-heading t)
        (let* ((beg (point))
               (end (save-excursion (org-end-of-subtree t t) (point)))
               (level (org-current-level))
               (stars (make-string level ?*)))
          (delete-region beg end)
          (goto-char beg)
          (insert stars " Reply (abandoned — " reason ") "
                  (format-time-string "[%Y-%m-%d %a %H:%M]")
                  "\n")
          (save-excursion
            (goto-char beg)
            (agent-shell-org-loops--add-sent-tag)))))))

(defun agent-shell-org-loops--recover-stranded ()
  "Clear stranded AGENT_PENDING entries whose shell buffer is gone.
Runs when a buffer enables `agent-shell-org-loops-mode' — typically on file
open — so a crash mid-turn doesn't leave the file wedged."
  (save-excursion
    (goto-char (point-min))
    (org-map-entries
     (lambda ()
       (when-let* ((uuid (org-entry-get (point) "AGENT_PENDING"))
                   (bname (org-entry-get (point) "AGENT_SHELL_BUFFER"))
                   ((not (get-buffer bname))))
         (agent-shell-org-loops--abandon-placeholder uuid "shell died")
         (org-entry-delete (point) "AGENT_PENDING"))))))

;;;; Batch trigger

;;;###autoload
(defun agent-shell-org-loops-run-all-ready-in-buffer ()
  "Trigger every heading in the current buffer whose state is the trigger."
  (interactive)
  (unless (agent-shell-org-loops--loops-enabled-p)
    (user-error "This buffer does not have `#+LOOPS: true'"))
  (agent-shell-org-loops--run-all-in-region (point-min) (point-max)))

;;;###autoload
(defun agent-shell-org-loops-run-all-ready-in-subtree ()
  "Trigger every trigger-state heading inside the current subtree."
  (interactive)
  (unless (agent-shell-org-loops--loops-enabled-p)
    (user-error "This buffer does not have `#+LOOPS: true'"))
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (agent-shell-org-loops--run-all-in-region beg end))))

(defun agent-shell-org-loops--run-all-in-region (beg end)
  "Trigger every trigger-state heading between BEG and END."
  (let (markers)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (when (and (equal (org-get-todo-state)
                          agent-shell-org-loops-trigger-state)
                   (not (member "noloop" (org-get-tags nil t))))
          (push (point-marker) markers))))
    (dolist (mk (nreverse markers))
      (save-excursion
        (goto-char mk)
        (agent-shell-org-loops--trigger-heading)))
    (message "Triggered %d heading(s)" (length markers))))

;;;; Dashboard

(defvar agent-shell-org-loops-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-shell-org-loops-list-visit)
    (define-key map (kbd "g")   #'agent-shell-org-loops-list-refresh)
    map)
  "Keymap for `agent-shell-org-loops-list-mode'.")

(define-derived-mode agent-shell-org-loops-list-mode tabulated-list-mode "Loops"
  "Dashboard listing every in-flight turn and pending permission."
  (setq tabulated-list-format
        [("Kind"    11 t)
         ("Heading" 42 t)
         ("File"    22 t)
         ("Elapsed"  9 nil)
         ("Shell"   28 nil)])
  (setq tabulated-list-entries #'agent-shell-org-loops--list-entries)
  (tabulated-list-init-header))

(defun agent-shell-org-loops--marker-heading (mk)
  "Return the heading title at marker MK, or nil."
  (when (and (markerp mk) (buffer-live-p (marker-buffer mk)))
    (with-current-buffer (marker-buffer mk)
      (save-excursion
        (goto-char mk)
        (ignore-errors (org-get-heading t t t t))))))

(defun agent-shell-org-loops--marker-file (mk)
  "Return the short file name for marker MK."
  (or (when (and (markerp mk) (buffer-live-p (marker-buffer mk)))
        (with-current-buffer (marker-buffer mk)
          (if buffer-file-name
              (file-name-nondirectory buffer-file-name)
            (buffer-name))))
      "(gone)"))

(defun agent-shell-org-loops--format-elapsed (started)
  "Format seconds since STARTED as HH:MM:SS."
  (if started
      (format-seconds "%.2h:%.2m:%.2s" (- (float-time) started))
    "-"))

(defun agent-shell-org-loops--list-entries ()
  "Compute `tabulated-list-entries' from the shell registry."
  (let (rows)
    (maphash
     (lambda (shell pl)
       (when (buffer-live-p shell)
         (dolist (turn (plist-get pl :pending-queue))
           (let ((mk (plist-get turn :origin)))
             (push
              (list (list :turn shell turn)
                    (vector "turn"
                            (or (agent-shell-org-loops--marker-heading mk)
                                "(gone)")
                            (agent-shell-org-loops--marker-file mk)
                            (agent-shell-org-loops--format-elapsed
                             (plist-get turn :started))
                            (buffer-name shell)))
              rows)))
         (maphash
          (lambda (uuid rec)
            (let ((mk (plist-get rec :origin)))
              (push
               (list (list :perm shell uuid rec)
                     (vector "permission"
                             (or (plist-get rec :title) "(untitled)")
                             (agent-shell-org-loops--marker-file mk)
                             "-"
                             (buffer-name shell)))
               rows)))
          (plist-get pl :permissions))))
     agent-shell-org-loops--shell-registry)
    (nreverse rows)))

;;;###autoload
(defun agent-shell-org-loops-list ()
  "Show a dashboard of every in-flight turn and pending permission."
  (interactive)
  (let ((buf (get-buffer-create "*agent-shell-org-loops*")))
    (with-current-buffer buf
      (agent-shell-org-loops-list-mode)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun agent-shell-org-loops-list-refresh ()
  "Recompute the dashboard entries."
  (interactive)
  (tabulated-list-print t))

(defun agent-shell-org-loops-list-visit ()
  "Jump to the origin heading of the entry at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (mk (pcase id
                    (`(:turn ,_shell ,turn) (plist-get turn :origin))
                    (`(:perm ,_shell ,_uuid ,rec) (plist-get rec :origin)))))
    (when (buffer-live-p (marker-buffer mk))
      (pop-to-buffer (marker-buffer mk))
      (goto-char mk)
      (if (fboundp 'org-fold-show-context)
          (org-fold-show-context)
        (with-no-warnings (org-show-context))))))

;;;; Auto-enable on file open

(defun agent-shell-org-loops--maybe-enable ()
  "Enable the minor mode when the current org buffer opts in."
  (when (agent-shell-org-loops--loops-enabled-p)
    (agent-shell-org-loops-mode 1)))

;;;###autoload
(define-minor-mode agent-shell-org-loops-mode
  "Buffer-local mode: watch this org buffer for READY transitions."
  :lighter " Loops"
  (if agent-shell-org-loops-mode
      (progn
        (add-hook 'org-after-todo-state-change-hook
                  #'agent-shell-org-loops--after-todo-state-change nil t)
        (add-to-list 'org-src-lang-modes '("agent-shell-permission" . fundamental))
        (unless (eq agent-shell-permission-responder-function
                    #'agent-shell-org-loops--permission-responder)
          (setq agent-shell-permission-responder-function
                #'agent-shell-org-loops--permission-responder))
        (agent-shell-org-loops--recover-stranded))
    (remove-hook 'org-after-todo-state-change-hook
                 #'agent-shell-org-loops--after-todo-state-change t)))

;;;###autoload
(define-globalized-minor-mode global-agent-shell-org-loops-mode
  agent-shell-org-loops-mode
  agent-shell-org-loops--maybe-enable
  :group 'agent-shell-org-loops
  (if global-agent-shell-org-loops-mode
      (add-hook 'org-capture-after-finalize-hook
                #'agent-shell-org-loops--after-capture-finalize)
    (remove-hook 'org-capture-after-finalize-hook
                 #'agent-shell-org-loops--after-capture-finalize)))

(provide 'agent-shell-org-loops)
;;; agent-shell-org-loops.el ends here
