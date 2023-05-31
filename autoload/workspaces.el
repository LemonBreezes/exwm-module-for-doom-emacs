;;; private/exwm/autoload/workspaces.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +exwm-workspaces ()
  "The list of EXWM workspaces created up to now.")

;;;###autoload
(defvar +exwm-floating-apps '("..." "virtualbox")
  "A list of class-names for EXWM applications which should stay floating.")

;;;###autoload
(defvar +exwm-workspace-name-replacements
  '(("google-chrome-unstable" . "Chrome")
    ("chromium-browser" . "Chrome")
    ("chromium" . "Chrome")
    ("firefox developer edition" . "Firefox")
    ("\"firefox developer edition\"" . "Firefox")
    ("\"firefoxdeveloperedition\"" . "Firefox")
    ("firefoxdeveloperedition" . "Firefox")
    ("kdeconnect." . "KDE Connect")
    ("libreoffice" . "Libreoffice")
    ("minecraft" . "Minecraft")
    ("plover" . "Plover")
    (".blueman-manager-wrapped" . "Blueman")
    ("discord" . "Discord")
    ("snes9x-gtk" . "Snes9x")
    (".epsxe-wrapped" . "ePSXe")
    ("qutebrowser" . "Qutebrowser")
    ("signal beta" . "Signal")
    ("net-runelite-client-runelite" . "RuneLite")
    ("gnome-control-center" . "Gnome CC")
    ("microsoft teams" . "Teams")
    ("virtualbox" . "VirtualBox")
    ("virtualbox manager" . "VirtualBox")
    ("virtualboxvm" . "VirtualBox")
    ("virtualbox machine" . "VirtualBox")
    ("wow.exe" . "WoW")
    ("battle.net.exe" . "Battle.net")
    ("hakuneko-desktop" . "Hakuneko")
    ("runescape" . "RuneScape"))
  "An alist whose for which a key is an EXWM class name and a value is the name
of the corresponding workspace that will be created.")

;;;###autoload
(defun exwm--disable-floating ()
            (unless (member exwm-class-name +exwm-floating-apps)
              (exwm-floating--unset-floating exwm--id)))

;;;###autoload
(defun +exwm-get-workspace-name (buffer)
  (let ((class (buffer-local-value 'exwm-class-name buffer)))
    (alist-get class +exwm-workspace-name-replacements
               class nil #'cl-equalp)))

;;;###autoload
(defun +exwm-persp--cleanup-windows (class-name windows)
  (--map-when (and (bufferp (window-buffer it))
                   (not (cl-equalp (+exwm-get-workspace-name
                                    (window-buffer it))
                                   class-name))
                   (window-live-p it)
                   (not (eq it (window-main-window)))
                   (not (memq (buffer-local-value 'major-mode
                                                  (window-buffer it))
                              '(term-mode
                                eshell-mode
                                vterm-mode))))
              (delete-window it)
              windows))

;;;###autoload
(defun +exwm-persp--after-match (buffer &rest _)
                (let* ((buffer (alist-get 'buffer state))
                       (application-name (+exwm-get-workspace-name buffer)))
                  (when (and (not (cl-equalp (+workspace-current-name)
                                             application-name))
                             (get-buffer-window buffer)
                             (window-live-p (get-buffer-window buffer))
                             (not (eq (get-buffer-window buffer)
                                      (window-main-window))))
                    (when (minibufferp)
                      (minibuffer-keyboard-quit))
                    (delete-window (get-buffer-window buffer)))
                  (cl-pushnew application-name +exwm-workspaces
                              :test #'string=)
                  (+workspace-switch application-name t)
                  (+workspace/display)

                  (+exwm-persp--cleanup-windows application-name (+popup-windows))
                  (when (+popup-window-p)
                    (other-window 1)
                    (switch-to-buffer buffer))
                  (+exwm-persp--cleanup-windows application-name (doom-visible-windows))))

;;;###autoload
(defun +exwm-persp--get-name (state)
             (setf (alist-get 'persp-name state)
                   (+exwm-get-workspace-name (alist-get 'buffer state)))
             state)

;;;###autoload
(defun +exwm-persp--predicate (buffer &optional state)
  (and (stringp (+exwm-get-workspace-name buffer))
       (or state t)))

;;;###autoload
(defun +exwm-persp--focus-workspace-app (&rest _)
            (when (and (cl-member (+workspace-current-name)
                                  +exwm-workspaces
                                  :test #'cl-equalp)
                       (or (not (boundp 'org-capture-mode))
                           (--none? (and (bufferp (window-buffer it))
                                         (buffer-local-value 'org-capture-mode
                                                             (window-buffer it)))
                                    (cl-union (+popup-windows)
                                              (doom-visible-windows)))))
              (let ((app-buffer
                     (--first (and (cl-equalp (+exwm-get-workspace-name it)
                                              (+workspace-current-name))
                                   (string= (+exwm-get-workspace-name it)
                                            (persp-name (get-current-persp))))
                              (cl-union (+workspace-buffer-list)
                                        (buffer-list)))))
                (unless (window-live-p (get-buffer-window app-buffer))
                  (when (+popup-window-p)
                    (other-window 1))
                  (switch-to-buffer app-buffer)))))

;;;###autoload
(defun +exwm-persp-cleanup-workspace ()
  "TODO"
  (when-let ((workspace (+exwm-get-workspace-name (current-buffer))))
    (when (persp-p (persp-get-by-name workspace))
      (let ((buffers
             (--filter (and (buffer-live-p it)
                            (not (eq it (current-buffer)))
                            (string= (+exwm-get-workspace-name it) workspace))
                       (persp-buffers (persp-get-by-name workspace)))))
        (unless buffers
          (+workspace-delete (+workspace-current))
          (+workspace/other))))))
