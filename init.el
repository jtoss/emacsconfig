(eval-after-load "org"
  '(require 'ox-md nil t))

;; enables odt export option for orgmode
(eval-after-load "org"
  '(require 'ox-odt nil t))

;; Automatic save session
;; (desktop-save-mode 1)

;; Keep the list of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; fix the problem of dead keys
(require 'iso-transl) 

(org-babel-do-load-languages
'org-babel-load-languages
'(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ocaml . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   (screen . t)
   (plantuml . t)
   (lilypond . t)
   ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(org-html-postamble-format (quote (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
