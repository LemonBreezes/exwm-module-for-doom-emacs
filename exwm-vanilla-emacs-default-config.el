;;; private/exwm/autoload/exwm-vanilla-emacs-default-config.el -*- lexical-binding: t; -*-

(setopt gc-cons-threshold ,gc-cons-threshold
        gc-cons-percentage ,gc-cons-percentage
        large-file-warning-threshold ,large-file-warning-threshold
        user-full-name ,user-full-name
        user-mail-address ,user-mail-address
        create-lockfiles nil
        inhibit-startup-screen t
        abbrev-file-name ,abbrev-file-name)

(with-eval-after-load 'org
  (setopt org-agenda-files ',(progn (require 'org)
                                    org-agenda-files)))

(set-language-environment "UTF-8")

;; Set up some editing conveniences
(electric-pair-mode 1)
(electric-indent-mode 1)
(show-paren-mode 1)
(global-eldoc-mode 1)
(abbrev-mode 1)
(setopt tab-always-indent 'complete)

;; Set up modeline and appearance
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(load-theme ',+exwm-vanilla-emacs-theme)

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
