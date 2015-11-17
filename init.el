;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General customizations

;;Add repositoris to be able to install packages
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")       package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")        package-archives) 

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)

;; Turn on auto-fill for all text buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; fix the problem of dead keys
(require 'iso-transl) 

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File extensions
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orgmode customization 

(eval-after-load "org"
  '(require 'ox-md nil t))
;; (eval-after-load "org"
;; This is not working ....
(load "~/.emacs.d/ox-md-patch.el")


;; enables odt export option for orgmode
(eval-after-load "org"
  '(require 'ox-odt nil t))

(eval-after-load "org"
  '(require 'ox-freemind nil t))


;; Automatic save session
;; (desktop-save-mode 1)

;; Keep the list of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Org-mode global bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;org-tree-slide settings
;; (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
;; (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)

;; Org-agenda commands ;;;;;;;;;;;;;;;
;; Start the angeda on the current day
;;(setq org-agenda-start-day "+0d")
;;(setq org-agenda-span "week")
(setq org-agenda-start-on-weekday nil)

;; Define some custom search tree 
(setq org-agenda-custom-commands
         '(
	   ;;("f" todo-tree "TODO")
	   ;;("U" tags-tree "meeting")
	   ;;("x" tags-tree "CATEGORY=\"phd\"")
	   ;;("X" org-match-sparse-tree "CATEGORY=\"phd\"")
	   ("c" . "CATEGORY") ; description for "c" prefix
	   ("ca" tags-tree "CATEGORY=\"admin\"")
	   ("co" tags-tree "CATEGORY=\"orgmode\"")
	   ("cp" tags-tree "CATEGORY=\"phd\"")
	   )
)

;;   '(("x" "Phd Tasks" (( tags-tree "CATEGORY=\"phd\""))))   
;;      '(("x" agenda )))

;; tentando fazer com que o emacs me de a lista de anexos para escolher. 

;; Isso faz o que eu quero
;;  https://lists.gnu.org/archive/html/emacs-orgmode/2014-09/msg00168.html
;; http://draketo.de/light/english/free-software/custom-link-completion-org-mode-25-lines-emacs

;; (defun org-attach-expand-julio (file)
;;   "Return a file link pointing to the current entry's attachment file FILE.
;; Basically, this adds the path to the attachment directory, and a \"file:\"
;; prefix."
;;   (interactive "P")
;;   (let* ((attach-dir (org-attach-dir t))
;; 	 (files (org-attach-file-list attach-dir))
;; 	 (file (if (= (length files) 1)
;; 		   (car files)
;; 		 (org-icompleting-read "Open attachment: "
;; 				       (mapcar 'list files) nil t))))
;;   (concat "file:" (org-attach-expand file))))

;; Link to attachment file
;; https://lists.gnu.org/archive/html/emacs-orgmode/2008-11/msg00108.html
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clocking work
(setq org-clock-into-drawer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 '(display-buffer-alist nil)
 '(markdown-command "pandoc")
 '(org-agenda-files (quote ("~/Copy/julio-personal.org" "~/Copy/Projects/ParVoronoi-wiki/graphprocessing.org" "~/Copy/Doutorado/activity-log.org")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda habit-down time-up priority-down category-keep) (todo category-up priority-down) (tags priority-down category-keep) (search category-keep))))
 '(org-html-postamble-format (quote (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
