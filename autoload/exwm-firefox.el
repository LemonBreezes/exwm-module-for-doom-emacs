;;; private/exwm/autoload/exwm-firefox.el -*- lexical-binding: t; -*-

;;;###autoload
(defun exwm-firefox-core-hint-links ()
  (interactive)
  (exwm-input--fake-key ?\M-j))

;;;###autoload
(defun exwm-firefox-core-hint-links-new-tab-and-switch ()
  (interactive)
  (exwm-input--fake-key ?\M-l))

;;;###autoload
(defun exwm-firefox-core-focus-first-input ()
  (interactive)
  (exwm-input--fake-key ?\M-j)
  (exwm-firefox-evil-insert))
