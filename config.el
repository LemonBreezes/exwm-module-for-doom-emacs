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

    ;; For some reason, after switching workspaces, input focus is not updated.
    ;; HACK This uses a mouse click to regain focus.
    (add-hook 'persp-activated-functions #'+exwm-refocus-application)

    ;; Show EXWM buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)))

;; Configure `exwm-randr' to support multi-monitor setups as well as
;; hot-plugging HDMI outputs. Read more at:
;; https://github.com/ch11ng/exwm/wiki#randr-multi-screen
(use-package! exwm-randr
  :after exwm
  :config
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
                    default-output)
                (with-temp-buffer
                  (call-process "xrandr" nil t nil)
                  (goto-char (point-min))
                  (re-search-forward xrandr-output-regexp nil 'noerror)
                  (setq default-output (match-string 1))
                  (forward-line)
                  (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
                      (call-process
                       "xrandr" nil nil nil
                       "--output" default-output
                       "--auto")
                    (call-process
                     "xrandr" nil nil nil
                     "--output" (match-string 1) "--primary" "--auto"
                     "--output" default-output "--off")
                    (setq exwm-randr-workspace-monitor-plist
                          (list 0 (match-string 1))))))))
  (exwm-randr-enable))

;; Configure emacs input methods in all X windows.
(when (featurep! +xim)
  (use-package! exwm-xim
    :after exwm
    :config
    ;; These variables are required for X programs to pick up Emacs IM.
    (setenv "XMODIFIERS" "@im=exwm-xim")
    (setenv "GTK_IM_MODULE" "xim")
    (setenv "QT_IM_MODULE" "xim")
    (setenv "CLUTTER_IM_MODULE" "xim")
    (setenv "QT_QPA_PLATFORM" "xcb")
    (setenv "SDL_VIDEODRIVER" "x11")
    (exwm-xim-enable)))

(use-package exwm-evil
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
        :n "u" #'exwm-firefox-core-tab-close-undo
        :n "U" #'exwm-firefox-core-undo
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

(use-package! exwm-mff
  :hook (exwm-init . exwm-mff-mode)
  :config
  (add-hook! 'persp-activated-functions (exwm-mff-warp-to-selected)))
