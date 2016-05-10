;; ELPA configuration and package installation
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(defvar required-packages
  '(magit
    helm
    monokai-theme))

;; activate installed packages
(package-initialize)

;; ensure default-packages are all installed
(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ============================================

;; Look and feel
(setq inhibit-startup-message t)
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(load-theme 'monokai t)

;; Text manipulation
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; TODO:
;; - ~/repositories/emacs.d/ and symlink
;; - manage emacs' backup files
