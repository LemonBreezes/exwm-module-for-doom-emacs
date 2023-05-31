;;; private/exwm/autoload/posframe-workarounds.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu--make-frame-a (oldfun &rest args)
  (cl-letf (((symbol-function #'frame-parent)
             (lambda (frame)
               (or (frame-parameter frame 'parent-frame)
                   exwm-workspace--current))))
    (apply oldfun args))
  (when exwm--connection
    (set-frame-parameter corfu--frame 'parent-frame nil)))

;;;###autoload
(defun +corfu--popup-redirect-focus-a ()
  (redirect-frame-focus corfu--frame
                        (or (frame-parent corfu--frame)
                            exwm-workspace--current)))

;;;###autoload
(defun +corfu-doc--redirect-focus ()
  (redirect-frame-focus corfu-doc--frame
                        (or (frame-parent corfu-doc--frame)
                            exwm-workspace--current)))

;;;###autoload
(defun +mini-popup--setup-frame-a (oldfun &rest args)
  (cl-letf (((symbol-function #'frame-parent)
             (lambda (frame)
               (or (frame-parameter frame 'parent-frame)
                   exwm-workspace--current))))
    (apply oldfun args))
  (when exwm--connection
    (set-frame-parameter mini-popup--frame 'parent-frame nil)))

;;;###autoload
(defun +corfu-doc--make-frame-a (oldfun &rest args)
  (cl-letf (((symbol-function #'frame-parent)
             (lambda (frame)
               (or (frame-parameter frame 'parent-frame)
                   exwm-workspace--current))))
    (apply oldfun args))
  (when exwm--connection
    (set-frame-parameter corfu-doc--frame 'parent-frame nil)))
