;;; agent-shell-org-loops-test.el --- Tests for agent-shell-org-loops -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make test'.  agent-shell is not needed: the few functions the
;; package calls are stubbed per test, and bliss is a fake script that
;; records its arguments.

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar agent-shell-permission-responder-function nil)
(require 'agent-shell-org-loops)
(require 'agent-shell-org-loops-bliss)

(setq org-todo-keywords
      '((sequence "TODO" "READY" "INPROGRESS" "PERMISSION" "NEEDINFO"
                  "|" "DONE" "CANCELLED")))

;;;; Helpers

(defvar loops-test--dir nil "Scratch directory of the running test.")

(defun loops-test--wait (pred &optional seconds)
  "Process events until PRED is true or SECONDS (default 5) pass."
  (let ((end (+ (float-time) (or seconds 5))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.02))
    (funcall pred)))

(defun loops-test--file (name)
  "Contents of NAME in the scratch directory, or nil."
  (let ((f (expand-file-name name loops-test--dir)))
    (when (file-exists-p f)
      (with-temp-buffer (insert-file-contents f) (buffer-string)))))

(defmacro loops-test--with-org (text &rest body)
  "Run BODY in a LOOPS org buffer holding TEXT, point on the first heading.
Quick tags are bound to a small fixed set inside a scratch directory."
  (declare (indent 1))
  `(let* ((loops-test--dir (make-temp-file "loops-test" t))
          (agent-shell-org-loops-projects
           `(("one" :key ?1 :directory ,(expand-file-name "p1" loops-test--dir)
              :related ("~/git/other" "a-package"))
             ("two" :key ?2 :directory ,(expand-file-name "p2" loops-test--dir))))
          (agent-shell-org-loops-approaches
           '(("research" :key ?r :instructions "Look, change nothing.")
             ("fix" :key ?f :edits t :instructions "Fix it.")))
          (agent-shell-org-loops-runners
           '(("local" :key ?l)
             ("tmux" :key ?t :bliss "here")
             ("pod" :key ?p :bliss "local" :unattended t)))
          (agent-shell-org-loops-notify-on-complete nil)
          (agent-shell-org-loops-bliss--turns nil))
     (make-directory (expand-file-name "p1" loops-test--dir))
     (make-directory (expand-file-name "p2" loops-test--dir))
     (unwind-protect
         (with-temp-buffer
           (insert "#+LOOPS: true\n" ,text)
           (org-mode)
           (agent-shell-org-loops-mode 1)
           (goto-char (point-min))
           (re-search-forward org-heading-regexp)
           (beginning-of-line)
           ,@body)
       (delete-directory loops-test--dir t))))

(defmacro loops-test--with-stub-shell (&rest body)
  "Run BODY with agent-shell stubbed.
Each started shell is a fresh buffer whose `default-directory' is the one
it was started in; `sent' collects the prompts."
  (declare (indent 0))
  `(let ((sent nil) (n 0))
     (ignore sent)
     (cl-letf* ((agent-shell-org-loops-agent-configs '(("claude-code" . ignore)))
                ((symbol-function 'agent-shell--start)
                 (lambda (&rest _)
                   (let ((dir default-directory)
                         (buf (generate-new-buffer
                               (format "*Claude* %d" (cl-incf n)))))
                     (with-current-buffer buf (setq default-directory dir))
                     buf)))
                ((symbol-function 'agent-shell-subscribe-to) #'ignore)
                ((symbol-function 'agent-shell-org-loops--send-or-queue)
                 (lambda (_shell prompt) (push prompt sent))))
       ,@body)))

(defun loops-test--fake-bliss (&rest overrides)
  "Write a fake bliss into the scratch directory and return its command.
It records arguments and the working directory of `ai' runs, and each
`stop'.  OVERRIDES is a plist: :ai-exit, :finished, :logs, :in-place."
  (let ((fake (expand-file-name "bliss" loops-test--dir))
        (d loops-test--dir))
    (with-temp-file fake
      (insert "#!" (or (executable-find "sh") "/bin/sh") "\n"
              "d=" (shell-quote-argument d) "\n"
              "case \"$1 $2 $3 $4\" in\n"
              "  *' ai '*)\n"
              "    for a in \"$@\"; do printf '%s\\n' \"$a\"; done > \"$d/args\"\n"
              "    pwd > \"$d/cwd\"\n"
              (if (plist-get overrides :ai-exit)
                  (format "    echo 'bliss: error: no such target' >&2; exit %d ;;\n"
                          (plist-get overrides :ai-exit))
                (concat "    echo 'bliss: started sunny-otter on here' >&2\n"
                        "    echo '{\"name\": \"sunny-otter\", \"target\": \"here\"}' ;;\n"))
              "  'status --json '*)\n"
              (format "    echo '{\"name\": \"sunny-otter\", \"state\": \"x\", \"finished\": %s}' ;;\n"
                      (if (plist-get overrides :unfinished) "false" "true"))
              "  'list --json '*)\n"
              (format "    echo '[{\"name\": \"sunny-otter\", \"in_place\": %s}]' ;;\n"
                      (if (plist-get overrides :remote) "false" "true"))
              "  'logs '*)\n"
              "    printf '%s\\n' "
              (shell-quote-argument
               (or (plist-get overrides :logs)
                   "Found it in \e[1mparser\e[0m.\nSTATE: DONE\n[bliss] command exited with status 0"))
              " ;;\n"
              "  'stop '*) echo \"$2\" >> \"$d/stops\" ;;\n"
              "esac\n"))
    (set-file-modes fake #o755)
    (list fake)))

(defmacro loops-test--with-bliss (overrides &rest body)
  "Run BODY with the fake bliss built from OVERRIDES and fast polling."
  (declare (indent 1))
  `(let ((agent-shell-org-loops-bliss-command (loops-test--fake-bliss ,@overrides))
         (agent-shell-org-loops-bliss-poll-interval 0.05))
     ,@body))

;;;; Quick tags

(ert-deftest loops-quick-tags-are-one-exclusive-group-per-kind ()
  (loops-test--with-org "* TODO x\n"
    (should (equal (agent-shell-org-loops--quick-tag-alist)
                   '((:startgroup) ("one" . ?1) ("two" . ?2) (:endgroup) (:newline)
                     (:startgroup) ("research" . ?r) ("fix" . ?f) (:endgroup) (:newline)
                     (:startgroup) ("local" . ?l) ("tmux" . ?t) ("pod" . ?p)
                     (:endgroup) (:newline))))))

(ert-deftest loops-capture-setup-offers-quick-tags-locally ()
  (loops-test--with-org "* TODO x\n"
    (with-temp-buffer
      (org-mode)
      (agent-shell-org-loops-capture-setup)
      (should (assoc "research" org-current-tag-alist))
      (should (local-variable-p 'org-current-tag-alist)))))

(ert-deftest loops-capture-setup-does-nothing-without-tags ()
  (let ((agent-shell-org-loops-projects nil)
        (agent-shell-org-loops-approaches nil)
        (agent-shell-org-loops-runners nil))
    (with-temp-buffer
      (org-mode)
      ;; org-mode makes the tag alist local itself, so check our marks.
      (agent-shell-org-loops-capture-setup)
      (should-not (local-variable-p 'org-use-fast-tag-selection))
      (should-not (assoc "research" org-current-tag-alist)))))

;;;; Resolving and prompts

(ert-deftest loops-project-property-beats-tag-beats-keyword ()
  (loops-test--with-org "* TODO x :two:\n"
    (should (string-suffix-p "/p2" (agent-shell-org-loops--resolve-project)))
    (org-entry-put (point) "LOOPS_PROJECT" "/elsewhere")
    (should (equal (agent-shell-org-loops--resolve-project) "/elsewhere")))
  (loops-test--with-org "* TODO x\n"
    (save-excursion (goto-char (point-min)) (insert "#+LOOPS_PROJECT: /kw\n"))
    (should (equal (agent-shell-org-loops--resolve-project) "/kw"))))

(ert-deftest loops-prompt-expands-tags-and-drops-them-from-tags-line ()
  (loops-test--with-org "* TODO Task :one:research:keepme:\nBody.\n"
    (let ((prompt (car (agent-shell-org-loops--build-prompt))))
      (should (string-match-p "Approach:\nLook, change nothing." prompt))
      (should (string-match-p "Project directory: .*/p1" prompt))
      (should (string-match-p "Also part of this work: .*/git/other, a-package" prompt))
      (should (string-match-p "^Tags: keepme$" prompt)))))

(ert-deftest loops-stateless-prompt-sends-everything-with-its-preamble ()
  (loops-test--with-org "* TODO Task :done:\nOld body.\n** Reply :done:\nEarlier answer.\n"
    (let ((stateful (car (agent-shell-org-loops--build-prompt)))
          (stateless (car (agent-shell-org-loops--build-prompt t "PREAMBLE"))))
      (should-not (string-match-p "Earlier answer" stateful))
      (should (string-match-p "\\`PREAMBLE" stateless))
      (should (string-match-p "Earlier answer" stateless)))))

(ert-deftest loops-state-marker-strict-and-lenient ()
  (should (equal (agent-shell-org-loops--strip-state-marker "Hi\nSTATE: DONE")
                 '("DONE" . "Hi")))
  (should (equal (agent-shell-org-loops--strip-state-marker "Hi\nSTATE: DONE\nnoise")
                 '(nil . "Hi\nSTATE: DONE\nnoise")))
  (should (equal (agent-shell-org-loops--strip-state-marker
                  "Hi\nSTATE: NEEDINFO\nSTATE: DONE\nnoise" t)
                 '("DONE" . "Hi\nSTATE: NEEDINFO"))))

;;;; Local agent-shell runs

(ert-deftest loops-local-run-links-the-shell-and-starts-in-project ()
  (loops-test--with-org "* TODO Task :one:local:\n"
    (loops-test--with-stub-shell
      (org-todo "READY")
      (let ((shell (get-buffer (org-entry-get (point) "AGENT_SHELL_BUFFER"))))
        (should (string-suffix-p "/p1/" (buffer-local-value 'default-directory shell)))
        (should (equal (org-get-todo-state) "INPROGRESS"))
        (re-search-forward "\\[\\[agent-shell:")
        (should (equal (org-element-property :path (org-element-context))
                       (buffer-name shell)))
        (save-window-excursion
          (org-open-at-point)
          (should (eq (window-buffer) shell)))
        (kill-buffer shell)))))

(ert-deftest loops-reply-keeps-the-link ()
  (loops-test--with-org "* TODO Task :local:\n"
    (loops-test--with-stub-shell
      (org-todo "READY")
      (let* ((shell (get-buffer (org-entry-get (point) "AGENT_SHELL_BUFFER")))
             (turn (car (plist-get (agent-shell-org-loops--tracking shell)
                                   :pending-queue))))
        (should turn)
        (agent-shell-org-loops--complete-turn turn "All good.\nSTATE: DONE")
        (should (string-match-p "^\\*\\* Reply \\[.*\\] \\[\\[agent-shell:" (buffer-string)))
        (should (equal (org-get-todo-state) "DONE"))
        (kill-buffer shell)))))

(ert-deftest loops-changed-project-gets-a-new-shell ()
  (loops-test--with-org "* TODO Task :one:\n"
    (loops-test--with-stub-shell
      (cl-flet ((run ()
                  (org-todo "READY")
                  (prog1 (org-entry-get (point) "AGENT_SHELL_BUFFER")
                    (org-entry-delete (point) "AGENT_PENDING"))))
        (let ((first (run)))
          (should (equal (run) first))
          (org-toggle-tag "one" 'off)
          (org-toggle-tag "two" 'on)
          (let ((second (run)))
            (should-not (equal second first))
            (should (string-suffix-p
                     "/p2/" (buffer-local-value 'default-directory
                                                (get-buffer second))))))))))

;;;; bliss runs

(ert-deftest loops-bliss-run-end-to-end ()
  (loops-test--with-org "* TODO Find bug :one:research:tmux:\nOn unicode.\n"
    (loops-test--with-bliss ()
      (org-todo "READY")
      (should (loops-test--wait (lambda () (string-match-p "Reply \\[" (buffer-string)))))
      (should (equal (org-get-todo-state) "DONE"))
      (should-not (org-entry-get (point) "AGENT_PENDING"))
      (should-not (org-entry-get (point) "BLISS_WORKLOAD"))
      (should (string-match-p "\\[\\[bliss:sunny-otter\\]" (buffer-string)))
      (should (string-match-p "Found it in parser\\.\n" (buffer-string)))
      (should-not (string-match-p "\\[bliss\\] command exited" (buffer-string)))
      (let ((args (loops-test--file "args")))
        (should (equal (seq-take (split-string args "\n") 5)
                       '("--target" "here" "ai" "--json" "--")))
        (should (string-match-p "nobody can answer" args)))
      (should (string-suffix-p "/p1\n" (loops-test--file "cwd")))
      ;; In place, so it is stopped once the reply is in.
      (should (loops-test--wait (lambda () (loops-test--file "stops")))))))

(ert-deftest loops-bliss-run-on-remote-target-is-not-stopped ()
  (loops-test--with-org "* TODO Task :pod:\n"
    (loops-test--with-bliss (:remote t)
      (org-todo "READY")
      (should (loops-test--wait (lambda () (string-match-p "Reply \\[" (buffer-string)))))
      (should (member "--unattended" (split-string (loops-test--file "args") "\n")))
      (accept-process-output nil 0.3)
      (should-not (loops-test--file "stops")))))

(ert-deftest loops-bliss-failure-abandons-the-turn ()
  (loops-test--with-org "* TODO Task :tmux:\n"
    (loops-test--with-bliss (:ai-exit 2)
      (org-todo "READY")
      (should (loops-test--wait (lambda () (string-match-p "abandoned" (buffer-string)))))
      (should (string-match-p "bliss failed: bliss: error: no such target" (buffer-string)))
      (should (equal (org-get-todo-state) "NEEDINFO"))
      (should-not (org-entry-get (point) "AGENT_PENDING")))))

(ert-deftest loops-bliss-edits-without-unattended-asks-first ()
  (loops-test--with-org "* TODO Task :fix:tmux:\n"
    (loops-test--with-bliss ()
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (org-todo "READY"))
      (should (equal (org-get-todo-state) "TODO"))
      (should-not (loops-test--file "args")))))

(ert-deftest loops-bliss-cancel-stops-a-workload-never-resumed ()
  (loops-test--with-org "* TODO Task :tmux:\n"
    (loops-test--with-bliss ()
      (org-entry-put (point) "AGENT_PENDING" "abc")
      (org-entry-put (point) "BLISS_WORKLOAD" "sunny-otter")
      (setq agent-shell-org-loops-bliss--turns nil)
      (org-todo "CANCELLED")
      (should (loops-test--wait (lambda () (loops-test--file "stops"))))
      (should-not (org-entry-get (point) "BLISS_WORKLOAD"))
      (should-not (org-entry-get (point) "AGENT_PENDING")))))

(ert-deftest loops-bliss-resumes-on-reopen ()
  (loops-test--with-org "* INPROGRESS Task :tmux:\n"
    (loops-test--with-bliss ()
      (org-entry-put (point) "AGENT_PENDING" "abc")
      (org-entry-put (point) "BLISS_WORKLOAD" "sunny-otter")
      (save-excursion
        (agent-shell-org-loops--insert-placeholder "abc"))
      (agent-shell-org-loops-mode -1)
      (agent-shell-org-loops-mode 1)
      (should (loops-test--wait (lambda () (string-match-p "Reply \\[" (buffer-string)))))
      (should (equal (org-get-todo-state) "DONE")))))

(provide 'agent-shell-org-loops-test)
;;; agent-shell-org-loops-test.el ends here
