;;; private/exwm/config.el -*- lexical-binding: t; -*-

(add-hook 'exwm-update-title-hook #'+exwm-rename-buffer-to-title)

(defun +exwm-init ()
  (use-package! exwm
    :config
    (exwm-enable)
    (when (featurep! +systemtray)
      (require 'exwm-systemtray)
      (exwm-systemtray-enable))

    ;; Fixes an error
    (when (featurep! :editor evil)
      (advice-add #'evil-refresh-cursor :before-until
                  (lambda (&rest _) (derived-mode-p 'exwm-mode))))

    ;; Prevent EXWM buffers from changing their name while not focused.
    ;; This allows Persp to restore them as intended.
    (when (featurep! :ui workspaces)
      (advice-add #'exwm--update-utf8-title :around
                  #'exwm--update-utf8-title-advice))

    ;; HACK Uses a mouse click to refocus the application.
    (add-hook 'persp-activated-functions #'+exwm-refocus-application)

    ;; Show `exwm' buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)))

(use-package! exwm-evil
  :when (featurep! :editor evil)
  :after exwm
  :config
  (exwm-evil-enable-mouse-workaround)
  (add-hook 'exwm-manage-finish-hook 'exwm-evil-mode)
  (cl-pushnew 'escape exwm-input-prefix-keys)
  (cl-pushnew ?\C-c exwm-input-prefix-keys)
  (map! :map exwm-evil-mode-map
        ;; We need a way to send Escape and C-c keys to the EXWM application.
        :prefix "C-c"
        :desc "Send Escape" "C-i" (cmd! (exwm-input--fake-key 'escape))
        :desc "Send Control-C" "C-c" (cmd! (exwm-input--fake-key ?\C-c))))

(use-package! exwm-firefox-evil
  :when (featurep! :editor evil)
  :after exwm
  :config
  (cl-pushnew 'escape exwm-input-prefix-keys)
  ;; We can use Vim keys with any browser that has compatible keybindings.
  (cl-loop for class in '("firefoxdeveloperedition"
                          "\"firefoxdeveloperedition\""
                          "IceCat"
                          "chromium-browser"
                          "Google-chrome"
                          "Google-chrome-unstable")
           do (cl-pushnew class exwm-firefox-evil-firefox-class-name))

  (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
  (map! :map exwm-firefox-evil-mode-map
        :n "gi" #'exwm-firefox-core-focus-first-input
        :n "f" #'exwm-firefox-core-hint-links
        :n "F" #'exwm-firefox-core-hint-links-new-tab-and-switch
        :after exwm-evil
        ;; This way we can use prefix arguments with these commands
        :n "j" #'exwm-evil-core-down
        :n "k" #'exwm-evil-core-up
        :n "h" #'exwm-evil-core-left
        :n "l" #'exwm-evil-core-right
        :n "+" #'exwm-evil-core-zoom-in
        :n "-" #'exwm-evil-core-zoom-out
        :n "=" #'exwm-evil-core-reset-zoom))

(use-package! exwm-edit
  :after exwm
  :config
  (setq exwm-edit-split "below")
  (add-hook! '(exwm-edit-before-finish-hook
               exwm-edit-before-cancel-hook)
    (defun exwm-edit-clear-last-kill ()
      (setq exwm-edit-last-kill nil)))
  (add-hook 'exwm-edit-compose-hook #'exwm-edit-activate-appropriate-major-mode))
