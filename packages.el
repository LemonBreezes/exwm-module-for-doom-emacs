;; -*- no-byte-compile: t; -*-
;;; os/exwm/packages.el

(package! exwm)
(when (featurep! :editor evil)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil"))
  (package! exwm-firefox-evil))
(package! exwm-edit)
(package! language-detection)
(package! exwm-mff)
