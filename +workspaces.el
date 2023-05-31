;;; private/exwm/+workspaces.el -*- lexical-binding: t; -*-

(require 'dash)

(add-hook 'exwm-floating-setup-hook #'exwm--disable-floating)

(persp-def-auto-persp "EXWM"
                      :parameters '((dont-save-to-file . t))
                      :hooks '(exwm-manage-finish-hook)
                      :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                 (persp-add-buffer-on-find-file nil)
                                 persp-add-buffer-on-after-change-major-mode)
                      :switch 'window
                      :predicate #'+exwm-persp--predicate
                      :after-match #'+exwm-persp--after-match
                      :get-name #'+exwm-persp--get-name)

(advice-add #'+workspace-switch :after #'+exwm-persp--focus-workspace-app)

(add-hook 'kill-buffer-hook #'+exwm-persp-cleanup-workspace)
