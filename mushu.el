;;; mushu.el
;; An ssh connection manager (integrated with helm)
(require 'term)

(setq mushu-ssh-conf "~/.ssh/config")


(setq mushu-ssh-sessions '())

(defun mushu-get-hosts (file)
  "Take an ssh config file, and returns a list of hosts given in that file"
  (split-string
    (shell-command-to-string
    (concat "grep \"Host \" " file " | awk '{ print $2 }'"))))

(defun mushu-host-list ()
  "A function, which extracts the hosts from the configured ssh config file."
  (mushu-get-hosts mushu-ssh-conf))

(defun mushu-filter (pred lst)
  "mushu-filter is a utility function - it returns the elements of list lst
   which fulfil a predicate pred"
  (cond
   ((null lst) '())
   ((funcall pred (car lst))
      (cons (car lst) (mushu-filter pred (cdr lst))))
   (t (mushu-filter pred (cdr lst)))))

(defun mushu-string-subst (new old lst)
  "mushu-string-subst replaces the first occurence of old in lst with new"
  (cond
   ((null lst) '())
   ((string= old (car lst)) (cons new (cdr lst)))
   (t (cons (car lst) (mushu-string-subst new old (cdr lst))))))

(defun mushu-ssh-rename (name)
  "mushu-ssh-rename lets us rename an ssh session (assuming the current buffer
   corresponds to the buffer we want to rename). It renames both the buffer,
   and updates the name of the session in the session list"
  (let
      ((buf (buffer-name))
       (new-session-list
	(mushu-string-subst (rename-buffer name t) buf mushu-ssh-sessions)))
    (setq mushu-ssh-sessions new-session-list)))

(defun mushu-ssh-term (host)
  "(mushu-ssh-term host) opens a new ssh session to host, and returns
   the new buffer created."
  (let
      ((ssh-buffer-name
	(generate-new-buffer-name (concat "*" host "*")))
       (args (list host)))

    (set-buffer
     (apply 'term-ansi-make-term ssh-buffer-name "ssh" nil args))
    (term-mode)
    (term-char-mode)
    (add-to-list 'mushu-ssh-sessions ssh-buffer-name)
    (switch-to-buffer ssh-buffer-name)))

(defun mushu-helm-new-ssh-session ()
  "mushu-helm-new-ssh-session uses a helm menu to create a new ssh
   session to a host on the list (mushu-host-list)."
  (interactive)
  (let
      ((host-list (mushu-host-list))
      (helm-outline-hosts '((name . "Hosts")
			     (candidates . host-list)
			     (action . mushu-ssh-term))))
    (helm :sources '(helm-outline-hosts))))

(defun mushu-helm-switch-to-ssh-session ()
  "mushu-helm-switch-to-ssh-session uses a helm menu to select a live
   ssh session, and then switches to it."
  (interactive)
  ; First we filter the mushu-ssh-sessions so we only list
  ; those which are still live (have a buffer)
  (setq mushu-ssh-sessions (mushu-filter 'get-buffer mushu-ssh-sessions))
  (let
      ((helm-outline-current '((name . "Current SSH sessions")
			       (candidates . mushu-ssh-sessions)
			       (action . switch-to-buffer))))
    (helm :sources '(helm-outline-current))))

(defun mushu-helm-new-named-ssh-session (name)
  "mushu-helm-new-named-ssh-session first prompts for a name for
   a new ssh session, and then creates a connection to a server
   (which is selected using helm) from the list of hosts."
  (interactive "sSession name: ") 
  (let
      ((current (buffer-name))
       (new-buf (mushu-helm-new-ssh-session))
       (buf (buffer-name)))
    (cond  ; Check we actually made a new buffer
     ((string= current buf) (message "Error creating ssh buffer."))
     (t (mushu-ssh-rename name)))))

;;; end mushu.el
