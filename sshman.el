;; An ssh connection manager (integrated with helm)
(require 'term)
(setq host-list '("localhost"))

(setq ssh-sessions '())

;; filter is a utility function - it returns the elements of list lst
;; which fulfil a predicate pred
(defun filter (pred lst)
  (cond
   ((null lst) '())
   ((funcall pred (car lst))
      (cons (car lst) (filter pred (cdr lst))))
   (t (filter pred (cdr lst)))))

;; subst replaces the first occurence of old in lst with new
(defun subst (new old lst)
  (cond
   ((null lst) '())
   ((eq old (car lst)) (cons new (cdr lst)))
   (t (cons (car lst) (subst new old (cdr lst))))))

;; ssh-rename lets us rename an ssh session (assuming we are in the
;; buffer we want to rename). It both renames the buffer, and updates
;; the session list.
(defun ssh-rename (name)
  (let
      ((buf (buffer-name)))
    (rename-buffer name)
    (setq ssh-sessions (subst name buf ssh-sessions))))

  
;; (ssh-term host) opens a new ssh session to host, and returns
;; the new buffer created.
(defun ssh-term (host)
  (setq ssh-buffer-name (concat "*" host "*")) 
  (set-buffer (apply 'term-ansi-make-term ssh-buffer-name "ssh" nil (list host)))
  (term-mode)
  (term-char-mode)
  (add-to-list 'ssh-sessions ssh-buffer-name)
  (switch-to-buffer ssh-buffer-name))

;; helm-new-ssh-session uses a helm menu to create a new ssh session
;; with a host on the list host-list
(defun helm-new-ssh-session ()
  (interactive)
  (let
      ((helm-outline-hosts '((name . "Hosts")
			     (candidates . host-list)
			     (action . ssh-term))))
    (helm :sources '(helm-outline-hosts))))

;; helm-switch-to-ssh-session uses a helm menu to select a live ssh
;; session, and then switch to it.
(defun helm-switch-to-ssh-session ()
  (interactive)
  ; First we filter the ssh-sessions so we only list
  ; those which are still live (have a buffer)
  (setq ssh-sessions (filter 'get-buffer ssh-sessions))
  (let
      ((helm-outline-current '((name . "Current SSH sessions")
			       (candidates . ssh-sessions)
			       (action . switch-to-buffer))))
    (helm :sources '(helm-outline-current))))

;; helm-new-named-ssh-session first prompts for a name for
;; a new ssh session, and then creates a connection to a server
;; (selected with helm) from the list of hosts.
(defun helm-new-named-ssh-session (name)
  (interactive "sSession name: ") 
  (let
      ((current (buffer-name))
       (buf (helm-new-ssh-session)))
    (cond  ; Check we actually made a new buffer
     ((eq current buf) (message "Error creating ssh buffer."))
     (t (ssh-rename name)))))

;; Some keybindings to test with
(global-set-key (kbd "s-s") 'helm-switch-to-ssh-session)
(global-set-key (kbd "s-c") 'helm-new-ssh-session)
(global-set-key (kbd "C-s-c") 'helm-new-named-ssh-session)



