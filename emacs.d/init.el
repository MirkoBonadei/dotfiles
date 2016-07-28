;; ELPA configuration and package installation
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(defvar required-packages
  '(magit
    helm
    projectile
    erlang
    drag-stuff
    moe-theme))

;; activate installed packages
(package-initialize)

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ============================================

;; Look and feel
(require 'better-defaults)
(set-frame-font "Inconsolata 14")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))
;; Moe theme
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(moe-theme-set-color 'green)
;;(moe-light)
(moe-dark)

(set-face-foreground 'font-lock-comment-face "orange")

;; Helm
(helm-mode 1)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-candidate-number-limit 100
     helm-idle-delay 0.0
     helm-input-idle-delay 0.01
     helm-quick-update t
     helm-ff-skip-boring-files t)

;; Text manipulation
(drag-stuff-global-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist '(("" . "~/.emacs-saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; ============================================

;; Personal Home Page
(require 'ox-html)
(require 'ox-publish)
(setf user-full-name "Mirko Bonadei")
(setf user-mail-address "mirko.bonadei@gmail.com")
; (require 'ox-rss) (un-comment it later..)

;; Lots of settings have been set globally because at the moment I only 
;; use org-mode to publish my website. In case I will start to use it 
;; for other publishing projects I should move these configurations down 
;; in the org-publish-project-alist.
(setq org-export-html-coding-system 'utf-8-unix)
(setq org-tml-viewport nil)
(setq org-html-home/up-format "")
(setq org-export-with-toc nil) ;; No table of content
(setq org-export-with-author t)
(setq org-export-with-latex t)
(setq org-export-with-title t)
(setq org-export-with-section-numbers nil)
(setq org-export-headline-levels 4)
(setq org-export-default-language "en")
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-html-postamble-format 
      '(("en" "<hr /><p class=\"postamble\">Copyright %a (2010-2016). Last Updated %C.</p>")))
(setq org-html-head-extra 
        "
<link rel='stylesheet' type='text/css' href='/css/solarized-light.min.css' />
<link rel='stylesheet' type='text/css' href='/css/customizations.css' />")

(defun nav-bar (arg) 
  (format "<div class='nav'><a href='/'>HOME</a></div>"))

(setq org-publish-project-alist
      '(
        ("website"
         :base-directory "~/repositories/website/"
         :base-extension "org"
         :publishing-directory "~/repositories/website/.build/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :htmlized-source nil
         :html-preamble nav-bar
         :html-postamble t
         :auto-preamble t)
        ("blog"
         :base-directory "~/repositories/website/blog/"
         :base-extension "org"
         :publishing-directory "~/repositories/website/.build/blog/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :htmlized-source nil
         :html-preamble nav-bar
         :html-postamble t
         :auto-preamble t)
        ("static"
         :base-directory "~/repositories/website/"
         :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/repositories/website/.build/"
         :recursive t
         :htmlized-source nil
         :publishing-function org-publish-attachment)
        ("homepage" :components ("website" "static" "blog"))
        ))
