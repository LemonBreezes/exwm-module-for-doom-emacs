;;; os/exwm/autoload/exwm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Refocus input for the currently selected EXWM buffer, if any."
  (let ((input-delay 0.05))
    (run-at-time
     input-delay
     nil
     (defun +exwm-refocus-application--timer ()
       (when exwm-class-name
         (advice-add #'+exwm-refocus-application :override #'ignore)
         (run-at-time (* 2 input-delay) nil
                      (defun +exwm-refocus-application--cleanup ()
                        (advice-remove #'+exwm-refocus-application #'ignore)))
         (run-at-time input-delay nil
                      (lambda () (ignore-errors (throw 'exit #'ignore))))
         (read-string ""))))))

;;;###autoload
(defun +exwm-select-window-a (oldfun window &rest args)
                (when window (apply oldfun window args)))

;;;###autoload
(defun +exwm-rename-buffer-to-title ()
  "Rename the buffer to its `exwm-title'."
  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string= "gimp" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-title)))

;;;###autoload
(defun exwm--update-utf8-title-advice (oldfun id &optional force)
  "Only update the window title when the buffer is visible."
  (when (get-buffer-window (exwm--id->buffer id))
    (funcall oldfun id force)))

;;;###autoload
(defun +exwm-open-nested-emacs (arg)
  "Open a separate GUI instance of Emacs. If ARG is non-nil, create
a new workspace as well."
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs"
         (when arg (list "--debug-init"))))

;;;###autoload
(defun +exwm-open-nested-vanilla-emacs (arg)
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs" "-Q"
         "--eval" (+exwm-read-unquote-config +exwm-vanilla-emacs-config-file)
         (when arg (list "--debug-init"))))

(defun +exwm-read-unquote-config (fname)
  (thread-last
    `(backquote
      ,(let ((sexps nil))
         (with-temp-buffer
           (insert-file-contents fname)
           (condition-case _
               (while t (push (read (current-buffer)) sexps))
             (end-of-file))
           (setq sexps (nreverse sexps))
           (push 'progn sexps))))
    (eval)
    (prin1-to-string)))

;;;###autoload
(defun +exwm-vanilla-emacs-find-this-file ()
  (interactive)
  (+exwm-vanilla-emacs-find-file (buffer-file-name)))

;;;###autoload
(defun +exwm-vanilla-emacs-find-file (file)
  (interactive)
  (funcall #'start-process "Emacs" nil "emacs" "-Q"
         "--eval" (+exwm-read-unquote-config +exwm-vanilla-emacs-config-file)
         "--file" (or file
                      (read-file-name "Find file: " nil default-directory nil))))
