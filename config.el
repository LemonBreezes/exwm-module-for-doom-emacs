;;; os/exwm/config.el -*- lexical-binding: t; -*-

;; Only load this module inside the X Window System.
(when (eq 'x (framep (selected-frame)))

  (defvar +exwm-vanilla-emacs-config-file
    (expand-file-name "./exwm-vanilla-emacs-default-config.el" (doom-module-locate-path :private 'exwm))
    "The configuration loaded in our nested vanilla Emacs sessions.
The configuration must be a single symbolic expression as it is
handed off to. A macro syntax is valid. Macro
expansion occurs within the parent Emacs session.")
  (defvar +exwm-vanilla-emacs-theme
    (if (memq doom-theme '(modus-operandi modus-vivendi))
        doom-theme
      'wheatgrass)
    "The theme loaded in our vanilla Emacs child sessions.")

  (add-hook 'exwm-update-title-hook #'+exwm-rename-buffer-to-title)

  (use-package! exwm
    :config
    (use-package! exwm-randr
      :config
      (exwm-randr-enable))

    (use-package! exwm-systemtray
      :config
      (exwm-systemtray-enable))

    ;; A few `ido' fixes.
    (use-package! exwm-config
      :config
      (exwm-config--fix/ido-buffer-window-other-frame))

    ;; Using `helm-display-buffer-in-own-frame' causes EXWM to emit an error.
    (after! helm
      (when (eq helm-default-display-buffer-functions
                #'helm-display-buffer-in-own-frame)
        (setq! helm-default-prompt-display-function #'helm-default-display-buffer)))

    ;; Configure emacs input methods in all X windows.
    (use-package! exwm-xim
      :config
      ;; These variables are required for X programs to connect with XIM.
      (setenv "XMODIFIERS" "@im=exwm-xim")
      (setenv "GTK_IM_MODULE" "xim")
      (setenv "QT_IM_MODULE" "xim")
      (setenv "CLUTTER_IM_MODULE" "xim")
      (setenv "QT_QPA_PLATFORM" "xcb")
      (setenv "SDL_VIDEODRIVER" "x11")
      (exwm-xim-enable))

    (exwm-enable)

    ;; Never suspend Emacs when EXWM. Doing so locks up Emacs.
    (map! [remap suspend-frame] #'undefined)
    (advice-add #'suspend-frame :override #'ignore)

    ;; Prevent EXWM buffers from changing their name while not focused.
    ;; This allows Persp to restore them as intended.
    (when (featurep! :ui workspaces)
      (advice-add #'exwm--update-utf8-title :around
                  #'exwm--update-utf8-title-advice))

    ;; Show EXWM buffers in buffer switching prompts.
    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)

    (cl-pushnew ?\C-c exwm-input-prefix-keys)

    (map! :map exwm-mode-map
          :prefix "C-c"
          ;; Add Eshell-like keys for seding TAB and ESC to applications.
          :desc "Send Escape" "C-i" (cmd! (exwm-input--fake-key 'escape))
          :desc "Send C-c" "C-c" (cmd! (exwm-input--fake-key ?\C-c))
          ;; We also set up a separate general way to send prefix keys to our
          ;; application.
          :desc "Send the next key" "C-q" #'exwm-input-send-next-key)

    ;; Set up commands for running nested Emacs sessions
    (map! :leader
          (:prefix "o"
           :desc "Nested Emacs" "n" #'+exwm-open-nested-emacs
           :desc "Nested vanilla Emacs" "N" #'+exwm-open-nested-vanilla-emacs)
          (:prefix "f"
           :desc "Find file in vanilla Emacs" "v" #'+exwm-vanilla-emacs-find-file
           :desc "Find this file in vanilla Emacs" "V" #'+exwm-vanilla-emacs-find-this-file))

    ;; For people who run nested Emacs instances within EXWM.
    (setq! exwm-replace nil)

    ;; Fixes focus being lost from EXWM buffers when switching workspaces or
    ;; buffers. This seems to be a problem specific to Doom. There is probably a
    ;; more elegant solution but it requires more digging.
    (advice-add #'+workspace/switch-to :after #'+exwm-refocus-application)
    (add-hook 'doom-switch-buffer-hook #'+exwm-refocus-application)

    (when (featurep! :editor evil)
      (evil-set-initial-state 'exwm-mode 'emacs)
      (after! evil-snipe
        (add-to-list 'evil-snipe-disabled-modes 'exwm-mode))
      (cl-pushnew (aref (kbd doom-leader-alt-key) 0) exwm-input-prefix-keys))

    (when (featurep! :ui popup)
      (cl-pushnew ?\C-` exwm-input-prefix-keys))

    ;; Workarounds for packages utilizing childframes
    (advice-add #'corfu--make-frame :around #'+corfu--make-frame-a)
    (advice-add #'corfu--popup-redirect-focus :override
                #'+corfu--popup-redirect-focus-a)
    (advice-add #'corfu-doc--redirect-focus :override
                #'+corfu--popup-redirect-focus-a)
    (advice-add #'corfu-doc--make-frame :around #'+corfu-doc--make-frame-a)
    (advice-add #'mini-popup--setup-frame :around #'+mini-popup--setup-frame-a)

    ;; Remove `window-live-p' errors
    (advice-add #'select-window :around #'+exwm-select-window-a)

    ;; Remove invalid face errors
    (setq-hook! exwm-mode
      outline-minor-mode-highlight nil))

  (use-package! exwm-edit
    :after exwm
    :init
    (defvar exwm-edit-bind-default-keys nil)
    (map! :map exwm-mode-map
          :desc "Edit this input field in Emacs" "C-c '" #'exwm-edit--compose
          :localleader
          :desc "Edit this input field in Emacs" "'" #'exwm-edit--compose)
    :config
    (setq! exwm-edit-split "below"
           exwm-edit-yank-delay 0
           exwm-edit-paste-delay 0)

    ;; For some reason, `exwm-edit--yank' does not work for me reliably without
    ;; this.
    (advice-add #'exwm-edit--yank
                :override
                (defun +exwm-edit--yank ()
                  (delete-region (point-min) (point-max))
                  (insert (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
    (advice-add #'exwm-edit--compose
                :around
                (defun +exwm-edit--compose-a (oldfun &rest args)
                  (exwm-input--fake-key ?\C-a)
                  (run-at-time 0.06 nil #'exwm-input--fake-key ?\C-c)
                  (run-at-time 0.12 nil #'apply oldfun args)))
    (advice-add #'exwm-edit--send-to-exwm-buffer
                   :override
                   (defun +exwm-edit--send-to-exwm-buffer-a (text)
                     (exwm-edit--switch)
                     (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
                     (+exwm-refocus-application)
                     (run-at-time 0.07 nil (lambda () (exwm-input--fake-key ?\C-a)))
                     (run-at-time 0.12 nil
                                  (lambda (text)
                                    (kill-new text)
                                    (exwm-input--fake-key ?\C-v))
                                  text)))

    (add-hook! '(exwm-edit-before-finish-hook
                 exwm-edit-before-cancel-hook)
      (defun exwm-edit-clear-last-kill ()
        (setq exwm-edit-last-kill nil)))
    (add-hook 'exwm-edit-compose-hook #'+exwm-edit-activate-appropriate-major-mode))

  (use-package! exwm-mff
    :hook (exwm-init . exwm-mff-mode))

  (use-package! app-launcher
    :commands app-launcher-run-app
    :init
    (map! :leader :desc "Launch Linux app" "$" #'app-launcher-run-app)
    :config
    (defun +app-launcher--action-function-default-a (selected)
      (when (featurep! :ui workspaces)
        (when (string-match " ([a-zA-Z]+)$" selected)
          (setq selected (substring-no-properties selected 0 (match-beginning 0))))
        (+workspace-switch selected t)))
    (advice-add #'app-launcher--action-function-default :before
                #'+app-launcher--action-function-default-a))

  (use-package exwm-evil
    :when (featurep! :editor evil)
    :after exwm
    :config
    (exwm-evil-enable-mouse-workaround)
    (add-hook 'exwm-manage-finish-hook 'exwm-evil-mode)
    (cl-pushnew 'escape exwm-input-prefix-keys))

  (use-package! exwm-firefox-evil
    :when (featurep! :editor evil)
    :after exwm
    :config
    (cl-pushnew 'escape exwm-input-prefix-keys)
    ;; We can use VIM keys with any browser that has compatible keybindings.
    (cl-loop for class in '("firefoxdeveloperedition"
                            "\"firefoxdeveloperedition\""
                            "IceCat"
                            "chromium-browser"
                            "Chromium"
                            "Google-chrome"
                            "Google-chrome-unstable"
                            "Chromium-bin-browser-chromium"
                            "librewolf-default")
             do (cl-pushnew class exwm-firefox-evil-firefox-class-name
                            :test #'string=))

    (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)

    ;; Automatically reenable `evil-normal-state' after hitting `<return>'.
    (advice-add #'exwm-firefox-core-focus-search-bar
                :after
                (defun +exwm-firefox-core-focus-search-bar-a ()
                  (add-transient-hook! 'exwm-update-title-hook
                    (when exwm-firefox-evil-mode (exwm-firefox-evil-normal)))))
    (advice-add #'exwm-firefox-core-tab-new
                :after
                (defun +exwm-firefox-core-tab-new-a ()
                  (add-transient-hook! 'exwm-update-title-hook
                    (+exwm-firefox-core-focus-search-bar-a))))

    (map! :map exwm-firefox-evil-mode-map
          :n "f" #'exwm-firefox-core-hint-links ; Requires Link Hints add-on.
          :n "F" #'exwm-firefox-core-hint-links-new-tab-and-switch
          :n "u" #'exwm-firefox-core-tab-close-undo
          :n "U" #'exwm-firefox-core-undo
          :n "/" #'exwm-firefox-core-find ; Compatible with Chrome as well.
          ;; Do not accidentally send escape
          :n [remap exwm-firefox-core-cancel] #'exwm-evil-core-normal

          :after exwm-evil
          ;; These are mroe in line with Evil than the default
          :n "g0" #'exwm-firefox-core-tab-first
          :n "g$" #'exwm-firefox-core-tab-last
          :n "0"  #'exwm-evil-core-beginning-of-line
          :n "$"  #'exwm-evil-core-end-of-line
          ;; This way we can use prefix arguments with these commands.
          :n "j" #'exwm-evil-core-down
          :n "k" #'exwm-evil-core-up
          :n "h" #'exwm-evil-core-left
          :n "l" #'exwm-evil-core-right
          ;; Add zoom commands
          :n "+" #'exwm-evil-core-zoom-in
          :n "-" #'exwm-evil-core-zoom-out
          :n "=" #'exwm-evil-core-reset-zoom
          ;; Pass through some keybindings from Firefox
          :n "C-<next>"  #'exwm-firefox-core-tab-next
          :n "C-<prior>" #'exwm-firefox-core-tab-previous
          :n "<f6>" #'exwm-firefox-core-focus-search-bar)))
