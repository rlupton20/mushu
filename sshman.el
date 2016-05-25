;; An ssh connection manager (integrated with helm)
(require 'term)
(setq host-list '())

(setq ssh-sessions '())

;; filter is a utility function - it returns the elements of list lst
;; which fulfil a predicate pred
(defun filter (pred lst)
  (cond
   ((null lst) '())
   ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
   (t (filter pred (cdr lst)))))

;; (ssh-term host) opens a new ssh session to host
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
  (setq helm-outline-hosts
	'((name . "Hosts")
	  (candidates . host-list)
	  (action . ssh-term)))
  (helm :sources '(helm-outline-hosts)))

;; helm-switch-to-ssh-session uses a helm menu to select a live ssh
;; session, and then switch to it.
(defun helm-switch-to-ssh-session ()
  (interactive)
  ; First we filter the ssh-sessions so we only list
  ; those which are still live (have a buffer)
  (setq ssh-sessions (filter 'get-buffer ssh-sessions))
  (setq helm-outline-current
	'((name . "Current SSH sessions")
	  (candidates . ssh-sessions)
	  (action . choice)))
  (helm :sources '(helm-outline-current)))

;; Some keybindings to test with
(global-set-key (kbd "s-s") 'helm-switch-to-ssh-session)
(global-set-key (kbd "s-c") 'helm-new-ssh-session)
