(require 'package)
(package-initialize)
(setq package-archives
'( ;("ELPA" . "http://tromey.com/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("org" . "http://orgmode.org/elpa/" )
   ("melpa" . "http://melpa.org/packages/")
;;   ("marmalade" . "http://marmalade-repo.org/packages/")
)
)

;;(require 'org-install)
(require 'org)
;; (require 'org-html)

(setq auto-mode-alist
   (append (mapcar 'purecopy
      '(("\\.c$"   . c-mode)
	("\\.h$"   . c-mode)
	("\\.c.simp$" . c-mode)
	("\\.h.simp$" . c-mode)
	("\\.a$"   . c-mode)
	("\\.w$"   . cweb-mode)
	("\\.cc$"   . c++-mode)
	("\\.S$"   . asm-mode)
	("\\.s$"   . asm-mode)
	("\\.p$"   . pascal-mode)
	("\\.Rmd$" . poly-markdown-mode)
	("\\.pas$" . pascal-mode)
	("\\.tex$" . LaTeX-mode)
	("\\.txi$" . Texinfo-mode)
	("\\.el$"  . emacs-lisp-mode)
;;	("emacs"  . emacs-lisp-mode)
	("\\.ml[iylp]?" . tuareg-mode)
	("[mM]akefile" . makefile-mode)
	("[mM]akefile.*" . makefile-mode)
	("\\.mak" . makefile-mode)
	("\\.cshrc" . sh-mode)
	("\\.html$" . html-mode)
        ("\\.org$" . org-mode)
        ("\\.md$" . markdown-mode)
)) auto-mode-alist))

(setq inhibit-splash-screen t)

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

(load-theme 'tango-dark t)

;;(set-face-attribute 'default nil :font "DejaVu Sans Condensed")
(set-face-attribute 'default nil :font "DejaVu Sans Mono Book")
(set-face-attribute 'default nil :height 100)

  (global-font-lock-mode t)
  (custom-set-faces
    '(flyspell-incorrect ((t (:inverse-video t)))))
  ;;  (set-face-attribute 'flyspell-incorrect (t (:inverse-video t)))

(line-number-mode 1)
(column-number-mode 1)

(load-library "paren")
(show-paren-mode 1)
(transient-mark-mode t)
(require 'paren)

(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )

(global-set-key (kbd "C-x 4 t") 'swap-buffers-in-windows)

(setq recenter-positions '(middle 0.06 bottom))

(setq evil-want-C-i-jump nil)
(require 'evil)
(evil-mode 1)

(desktop-save-mode 1)

(server-start)

(global-set-key (kbd "C-c i") 
(lambda() (interactive)(org-babel-load-file "~/.emacs.d/init.org")))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

(global-set-key [f10] '(lambda () (interactive) (revert-buffer nil t nil)))

(global-set-key "\^x\^e" 'compile)

(defun jump-mark ()
  (interactive)
  (set-mark-command (point)))
(defun beginning-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (beginning-of-defun))
(defun end-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (end-of-defun))

(global-set-key "\^c\^b" 'beginning-of-defun-and-mark)
(global-set-key "\^c\^e" 'end-of-defun-and-mark)
(global-set-key "\^c\^j" 'jump-mark)
(global-set-key [S-f6] 'jump-mark)		;; jump from mark to mark

(global-set-key "\M-g" 'goto-line)

(setq select-active-regions nil)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)

;;  (if(string-equal system-type "gnu/linux")   ; Linux!
;;      (
       (require (quote xclip))
       (xclip-mode 1)
;;      )()
;;        )

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Inspired from http://tex.stackexchange.com/questions/166681/changing-language-of-flyspell-emacs-with-a-shortcut
;; (defun spell (choice)
;;    "Switch between language dictionaries."
;;    (interactive "cChoose:  (a) American | (f) Francais")
;;     (cond ((eq choice ?1)
;;            (setq flyspell-default-dictionary "american")
;;            (setq ispell-dictionary "american")
;;            (ispell-kill-ispell))
;;           ((eq choice ?2)
;;            (setq flyspell-default-dictionary "francais")
;;            (setq ispell-dictionary "francais")
;;            (ispell-kill-ispell))
;;           (t (message "No changes have been made."))) )

(define-key global-map (kbd "C-c s a") (lambda () (interactive) (ispell-change-dictionary "american")))
(define-key global-map (kbd "C-c s f") (lambda () (interactive) (ispell-change-dictionary "francais")))
(define-key global-map (kbd "C-c s r") 'flyspell-region)
(define-key global-map (kbd "C-c s b") 'flyspell-buffer)
(define-key global-map (kbd "C-c s s") 'flyspell-mode)

(defun auto-fill-mode-on () (TeX-PDF-mode 1))
(add-hook 'tex-mode-hook 'TeX-PDF-mode-on)
(add-hook 'latex-mode-hook 'TeX-PDF-mode-on)

(setq TeX-PDF-mode t)

(defun auto-fill-mode-on () (auto-fill-mode 1))

;; (add-hook 'text-mode-hook 'auto-fill-mode-on)
;; (add-hook 'emacs-lisp-mode 'auto-fill-mode-on)
;; (add-hook 'tex-mode-hook 'auto-fill-mode-on)
;; (add-hook 'latex-mode-hook 'auto-fill-mode-on)

  (setq c-default-style "k&r")
  (setq c-basic-offset 2)

(defalias 'yes-or-no-p 'y-or-n-p)

  (require 'iso-transl)

(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

(setq org-directory "~/org/")

(setq org-hide-leading-stars t)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
(setq org-startup-indented t)      ;; turn on org-indent-mode for all files
(setq org-log-into-drawer t)       ;; insert notes into :LOGBOOK: drawer 
;;(setq org-cycle-include-plain-lists 'integrate) ;; fold plain list when cycling heading visibility
(setq org-cycle-include-plain-lists (quote integrate))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file "~/org/notes.org")
     (define-key global-map "\C-cd" 'org-capture)
(setq org-capture-templates (quote (
("t" "Todo" entry (file+headline "~/org/teste.org" "%t" ) "* TODO %?
  %i
  %a" :prepend t) 
("j" "Journal" entry (file+datetree "~/org/teste.org") "* %?
Entered on %U
  %i
  %a"))))

(setq org-agenda-custom-commands
             '(("D" "Diary's todos" tags "diary/TODO|DONE"))
)

(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)
;;displays the agenda starting today
;;(setq org-agenda-start-on-weekday nil)
;;displays the agenda starting today
(setq org-agenda-start-on-weekday 1)

(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-sorting-strategy (quote 
((agenda habit-down time-up priority-down category-keep) (todo category-up priority-down) (tags priority-down category-keep) (search category-keep))))

(setq org-agenda-files (quote (
"~/Sync/Doutorado/activity-log.org"
"~/org/julio-personal.org"
)))

; Adds new file to track on the agenda
(push "~/Projects/hppsimulations/LabBook.org" org-agenda-files)
; (push "~/Projects/hppsimulations/WORKING_DOC/pma.org" org-agenda-files)

(push "~/Projects/hppsimulations/newpma/newpma.org" org-agenda-files)

(push "~/Copy/Doutorado/thesis_proposal/thesis_proposal.org" org-agenda-files) ; thesis proposal

(push "~/Copy/Projects/ParVoronoi-wiki/graphprocessing.org" org-agenda-files)

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

(setq org-clock-into-drawer t)

(eval-after-load 'ord-attach 
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link))))

(require 'org-inlinetask)

(setq org-ditaa-jar-path "/usr/bin/ditaa")

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(setq org-catch-invisible-edits nil)

(setq org-file-apps
'((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     (system . "setsid xdg-open %s")))

(global-set-key (kbd "C-c d") 'insert-date)
(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "** %Y-%m-%d")
                   ((equal prefix '(4)) "[%Y-%m-%d]"))))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c t") 'insert-time-date)
(defun insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                   ((equal prefix '(4)) "%Y%m%d%H%M%S"))))
      (insert (format-time-string format))))

(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c g") 'org-git-insert-link-interactively)

(global-set-key (kbd "C-c <up>") 'outline-up-heading)
(global-set-key (kbd "C-c <left>") 'outline-previous-visible-heading)
(global-set-key (kbd "C-c <right>") 'outline-next-visible-heading)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

;(global-set-key (kbd "C-x <dead-grave>") 'next-error)
;(global-set-key (kbd "C-x <S-dead-tilde>") 'previous-error)

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Evaluates src blocks during export (the header arguments at least)
(setq org-export-babel-evaluate t) 

;; Dont ask for confirmation on export 
(setq org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (C . t)
     (python . t)
 ;    (sh . t) ;; BUG for some reason this causes a conflict when exporting
     (shell . t)
     (R . t)
   ;  (ruby . t)
   ;  (ocaml . t)
     (ditaa . t)
   ;  (dot . t)
   ;  (octave . t)
   ;  (sqlite . t)
   ;  (perl . t)
   ;  (screen . t)
   ;  (plantuml . t)
   ;  (lilypond . t)
     (org . t)
     (makefile . t)
     (latex . t)
     ))
  (setq org-src-preserve-indentation t)

(add-to-list 'org-structure-template-alist
        '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("r" "#+begin_src R :results output :exports both :session ?\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session ?\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("p" "#+begin_src python :results output :exports both\n?\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("P" "#+begin_src python :results output :exports both :session\n?\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("ip" "#+begin_src ipython :exports both :results output :session\n?\n#+end_src" "<src lang=\"ipython\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("IP" "#+begin_src ipython :exports both :results output :session ?\n\n#+end_src" "<src lang=\"ipython\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("b" "#+begin_src sh :results output :exports both\n?\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("B" "#+begin_src sh :session ? :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("CPP" "#+begin_src C++ :flags -std=c++11 :includes <iostream> \n\n#+end_src" "<src lang=\"c++\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("C" "#+begin_src C :includes <stdio.h> \n\n#+end_src" "<src lang=\"c\">\n\n</src>"))

(global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images) 
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-babel-result-hide-all)

(setq python-shell-interpreter "python3")

(setq org-babel-python-command "python3")

(require 'ob-ipython)

(setq exec-path (append '("/home/julio/anaconda3/bin") exec-path))

(setq org-babel-use-quick-and-dirty-noweb-expansion t)

(require 'org-ref)
(setq reftex-default-bibliography '("~/Documents/Mendeley/library.bib"))

(setq 
 ;org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
 org-ref-default-bibliography '("~/Documents/Mendeley/library.bib")
 ;org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"
 )

;; Sets the path to my bibtex file (which is generated by Mendeley) 
(setq bibtex-completion-bibliography '("~/Documents/Mendeley/library.bib"))

;; Configure the field on the bibtex that contains the path to the pdf file.
(setq bibtex-completion-pdf-field "file")

;;Search bibtex
;;(global-set-key (kbd "C-c C-x [") 'helm-bibtex)

(eval-after-load "helm-bibtex" 
'(defun helm-bibtex-find-pdf-in-field (key-or-entry)
    "Returns the path of the PDF specified in the field
`helm-bibtex-pdf-field' if that file exists.  Returns nil if no
file is specified, or if the specified file does not exist, or if
`helm-bibtex-pdf-field' is nil."

    (when helm-bibtex-pdf-field
      (let* ((entry (if (stringp key-or-entry)
			(helm-bibtex-get-entry1 key-or-entry t)
		      key-or-entry))
	     (value (helm-bibtex-get-value helm-bibtex-pdf-field entry)))
	(cond
	 ((not value) nil)         ; Field not defined.
	 ((f-file? value) value)   ; A bare path was found.
	 (t				; Assuming Zotero/Mendeley/JabRef format:
	  (cl-loop  ; Looping over the files:
	   for record in (s-split ";" value)
	   for record = (s-split ":" record)
	   for file-name = (nth 0 record)
	   for path = (nth 1 record)
	   if (f-file? path)
	   collect (f-full path)
	   else if (f-file? (f-full (f-join path file-name)))
	   collect (f-full (f-join path file-name))
	   ;; This is to work around a bug in Mendeley.
	   else if (f-file? (concat "/" path))
	   collect ( concat "/" path))))))))

;;(require 'helm-bibtex)
;;(setq helm-bibtex-pdf-open-function
;;      (lambda (fpath)
;;	(start-process "mendeleydesktop" "*helm-bibtex-mendeleydesktop*" "/usr/bin/mendeleydesktop" fpath)))

;;(require 'helm-bibtex)
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
	(call-process "mendeleydesktop" nil 0 nil fpath)))

(require 'org)
(org-add-link-type "BIB" 'org-bib-open 
(lambda (path desc format)
  (cond
   ((eq format 'latex)
    (format "\\cite{%s}" path))))) 

;;(add-hook 'org-store-link-functions 'org-bib-store-link)

(defun org-bib-open (path)
  "Uses the helm functions to look up the path on the bibtex."
  (funcall bibtex-completion-pdf-open-function (nth 0 (bibtex-completion-find-pdf-in-field path ))))

(defun bibtex-completion-format-citation-BIB (keys)
  "Formatter for BIB references."
  (s-join ", "
          (--map (format "BIB:%s" it) keys)))

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-BIB)
	      (latex-mode    . bibtex-completion-format-citation-cite)
	      (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	      (default       . bibtex-completion-format-citation-default)))

 ;; Clear the default values for this class
 ;; (unless (boundp 'org-latex-classes) (setq org-latex-classes nil))

 (add-to-list 'org-latex-classes '("acm-proc-article-sp" "\\documentclass{acm_proc_article-sp}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f ; bibtex `basename %f | sed 's/\.tex//'` ; pdflatex -interaction nonstopmode -output-directory  %o %f ; pdflatex -interaction nonstopmode -output-directory %o %f"))

;(setq org-latex-to-pdf-process '("bibtex `basename %f | sed 's/\.tex//'`"))

(add-to-list 'org-latex-classes '("article" "\\documentclass{article}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

; (require 'ox-md nil t)
;(require 'ox-gfm nil t)
(require 'ox-pandoc)

(defun rst-replace-image-links ()
  "Fix image links after rst export."
  (goto-char (point-min))
  (while 
      (re-search-forward "image:: \\(file:\\)" nil t)
    (replace-match "" nil nil nil 1)
    )
  )

( add-hook 'org-pandoc-after-processing-rst-hook 'rst-replace-image-links )

(require 'tramp)
(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x g") 'magit-status)
