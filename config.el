;;; os/exwm/config.el -*- lexical-binding: t; -*-

(defvar +exwm-vanilla-emacs-config-file
  (expand-file-name "./exwm-vanilla-emacs-default-config.el" (doom-module-locate-path :private 'exwm))
  "The configuration loaded in our nested vanilla Emacs sessions.
The configuration must be a single symbolic expression as it is
handed off to. A macro syntax is valid. Macro
expansion occurs within the parent Emacs session.")
(defvar +exwm-vanilla-emacs-theme 'wheatgrass
  "The theme loaded in our vanilla Emacs child sessions.")
(defvar +exwm-nested-emacs-map (make-sparse-keymap)
    "The keymap we will use to override `exwm-mode-map' for nested Emacs sessions.")

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

  ;; HACK Fixes focus being lost from EXWM buffers when switching workspaces or
  ;; buffers. This seems to be a problem specific to Doom. There is probably a
  ;; more elegant solution but it requires more digging.
  (advice-add #'+workspace/switch-to :after #'+exwm-refocus-application)
  (add-hook 'doom-switch-buffer-hook #'+exwm-refocus-application)

  (define-key +exwm-nested-emacs-map (kbd "C-c")
    (cmd! (exwm-input--fake-key ?\C-c)))

  (add-hook 'exwm-update-class-hook #'+exwm-setup-nested-emacs-keys)

  (when (featurep! :editor evil)
    (evil-set-initial-state 'exwm-mode 'emacs)
    (after! evil-snipe
      (add-to-list 'evil-snipe-disabled-modes 'exwm-mode))
    (cl-pushnew (aref (kbd doom-leader-alt-key) 0) exwm-input-prefix-keys))
  
  (when (featurep! :ui popup)
    (cl-pushnew ?\C-` exwm-input-prefix-keys))

  ;; Workarounds for childframes
  (after! corfu
    (advice-add #'corfu--make-frame :around
                (defun +corfu--make-frame-a (oldfun &rest args)
                  (cl-letf (((symbol-function #'frame-parent)
                             (lambda (frame)
                               (or (frame-parameter frame 'parent-frame)
                                   exwm-workspace--current))))
                    (apply oldfun args))
                  (when exwm--connection
                    (set-frame-parameter corfu--frame 'parent-frame nil))))

    (advice-add #'corfu--popup-redirect-focus :override
                (defun +corfu--popup-redirect-focus-a ()
                  (redirect-frame-focus corfu--frame
                                        (or (frame-parent corfu--frame)
                                            exwm-workspace--current)))))
  (after! corfu-doc
    (advice-add #'corfu-doc--redirect-focus :override
                (defun +corfu-doc--redirect-focus ()
                  (redirect-frame-focus corfu-doc--frame
                                        (or (frame-parent corfu-doc--frame)
                                            exwm-workspace--current))))

    (advice-add #'corfu-doc--make-frame :around
                (defun +corfu-doc--make-frame-a (oldfun &rest args)
                  (cl-letf (((symbol-function #'frame-parent)
                             (lambda (frame)
                               (or (frame-parameter frame 'parent-frame)
                                   exwm-workspace--current))))
                    (apply oldfun args))
                  (when exwm--connection
                    (set-frame-parameter corfu-doc--frame 'parent-frame nil)))))

  (after! mini-popup
    (advice-add #'mini-popup--setup-frame :around
                (defun +mini-popup--setup-frame-a (oldfun &rest args)
                  (cl-letf (((symbol-function #'frame-parent)
                             (lambda (frame)
                               (or (frame-parameter frame 'parent-frame)
                                   exwm-workspace--current))))
                    (apply oldfun args))
                  (when exwm--connection
                    (set-frame-parameter mini-popup--frame 'parent-frame nil))))))

(use-package! exwm-edit
  :after exwm
  :init
  (defvar exwm-edit-bind-default-keys nil)
  (map! :map exwm-mode-map
        :desc "Edit this input field in Emacs" "C-c '" #'exwm-edit--compose)
  :config
  (setq! exwm-edit-split "below"
         exwm-edit-paste-delay 0.2)
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
                          "librewolf-default")
           do (cl-pushnew class exwm-firefox-evil-firefox-class-name
                          :test #'string=))

  (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
  (map! :map exwm-firefox-evil-mode-map
        :n "f" #'exwm-firefox-core-hint-links ; Requires Link Hints add-on.
        :n "F" #'exwm-firefox-core-hint-links-new-tab-and-switch
        :n "u" #'exwm-firefox-core-tab-close-undo
        :n "U" #'exwm-firefox-core-undo
        :n "/" #'exwm-firefox-core-find ; Compatible with Chrome as well.
        :n [remap exwm-firefox-core-cancel] #'+exwm-firefox-core-cancel
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
        :n "=" #'exwm-evil-core-reset-zoom))
