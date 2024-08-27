;;; .emacs.el --- This is my personal Emacs configuration file

;;; Commentary:
;; This package configure a customized Emacs configuration for note-taking and programming.
;; 
;; Usage:
;; Add this configuration file to the user.  The configuration file is holded in the user-init-file.

;;; The configuration references fot the current configuration locate to:

;;; - https://www.orgroam.com/manual.html
;;; - https://github.com/SystemCrafters/crafter-configs
;;; - https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;;; - https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
;;; - https://github.com/SystemCrafters/crafted-emacs
;;; - https://gitlab.com/dwt1/configuring-emacs

;;; Define and initialise package repositories
;;; Code:

(require 'package)
(add-to-list
        'package-archives
        '("melpa" . "https://melpa.org/packages/") t)

;;; Load Emacs Lisp packages, and activate them.
(package-initialize)

;;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package)
   )

(require 'use-package)

(setq use-package-always-ensure 't)

;;; The configuration of org-directory for notes saving.
(setq org-directory
      (concat
        (getenv "HOME")
        "/Documents/Dropbox/notes/")
)

;; Theme configurationn.
(use-package exotica-theme
  :config (load-theme 'exotica t)
)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
;;; To hide the bar
(tool-bar-mode -1)
;;; To hide the menu bar.
(menu-bar-mode -1)
;;; To hide scroll bar.
(scroll-bar-mode -1)

;;; To write y or n as response in a dialog.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Define the init file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory)
)

;;; If the file exists, load it.
(when (file-exists-p custom-file)
    (load custom-file)
)

;;; Set the sursor type to "|"
(setq cursor-type 'bar)

;; Helm configuration
(use-package helm
  :init
  (helm-mode 1)
  :bind
  (("M-x"     . helm-M-x) ;; Evaluate functions
   ("C-x C-f" . helm-find-files) ;; Open or create files
   ("C-x b"   . helm-mini) ;; Select buffers
   ("C-x C-r" . helm-recentf) ;; Select recently saved files
   ("C-c i"   . helm-imenu) ;; Select document heading
   ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
  :map helm-map
   ("C-z" . helm-select-action)
   ("<tab>" . helm-execute-persistent-action)
  )
)

(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.5
          which-key-idle-secondary-delay 0.5)
    (which-key-setup-side-window-bottom)
)

;; Auto completion
(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t)
  :hook (prog-mode . company-mode)
  )

;;; Enable company globally
(global-company-mode)

;;; Set default, fixed and variabel pitch fonts
;;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
					; :config
					; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
					; (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
					; (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")
)

(require 'solaire-mode)
(add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

;;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1)
)

;;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(500)
)

;;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode)
)

;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook
	    (lambda () (org-superstar-mode 1))
  )
)

;;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)

;;; Increase line spacing
(setq-default line-spacing 6)

;;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free))
)

;;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)
  
;;; Overwrite selected text
(delete-selection-mode t)
  
;;; Scroll to the first and last line of the buffer
(setq scroll-error-top-bottom t)

(setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))


;;; Org-Mode initial setup
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
  )
)

;;; From org-roam-protocol

(setq org-roam-capture-ref-templates '(
("r" "Roam Ref" plain ;;;(function org-roam-capture--get-point)
 "%?"
 :if-new
 (file+head "${slug}.org"
            "#+title: ${title}
#+date: %u
#+lastmod:
#+roam_key: r
#+roam_alias: \"Roam Ref\"
#+roam_tags: Roam

* ${title}
:PROPERTIES:
:URL: ${ref}
:CREATED: %U
:END:

${body}")
 :unnarrowed t)
)
)

;;; Org-Roam capture templates.
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new
         (file+head "${slug}.org"
                    "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
         :immediate-finish t)
("r" "Roam Ref" plain ;;;(function org-roam-capture--get-point)
 (file "~/.emacs.d/roam-notes-templates/ref-roam-template.org")
 :if-new
 (file+head "${slug}.org" "")
 :immediate-finish t)
	)
      time-stamp-start "#\\+lastmod: [\t]*"
)

;; Org-Roam basic configuration
(use-package org-roam
  :after org
  :ensure t
  :demand t  ;;; Ensure org-roam is loaded by default
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new
      (file+head "${slug}.org"
                 "#+title: ${title}\n#+date: %u\n#+lastmod: \n\n")
      :immediate-finish t)

     ("r" "Roam Ref" plain ;;;(function org-roam-capture--get-point)
      "%?"
      :if-new
      (file+head "${slug}.org"
		 "#+title: ${title}
#+date: %u
#+lastmod:
#+roam_key: r
#+roam_alias: \"Roam Ref\"
#+roam_tags: Roam

* ${title}
:PROPERTIES:
:URL: ${ref}
:CREATED: %U
:END:

${body}")
      :immediate-finish t)
     
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "~/org/roam_templates/book_note_template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)
     )
   time-stamp-start "#\\+lastmod: [\t]*")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-project)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n b" . my/org-roam-capture-inbox)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
	 )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  )

;; (use-package org-roam
;;     :after org
;;     :ensure t
;;     :demand t
;;     :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
;;     :custom
;;     (org-roam-directory (file-truename org-directory))
;;     :config
;;     (org-roam-db-autosync-mode)
;;     :bind (("C-c n f" . org-roam-node-find)
;;            ("C-c n r" . org-roam-node-random)
;;            (:map org-mode-map
;;                  (("C-c n i" . org-roam-node-insert)
;;                   ("C-c n o" . org-id-get-create)
;;                   ("C-c n t" . org-roam-tag-add)
;; 		  ("C-c n I" . org-roam-node-insert-immediate)
;;                   ("C-c n p" . my/org-roam-find-project)
;;                   ;;("C-c n t" . my/org-roam-capture-task)
;;                   ("C-c n b" . my/org-roam-capture-inbox)
;;                   ("C-c n a" . org-roam-alias-add)
;; 		  ("C-M-i" . completion-at-point)
;;                   ("C-c n l" . org-roam-buffer-toggle))
;; :map org-roam-dailies-map
;;          ("Y" . org-roam-dailies-capture-yesterday)
;;          ("T" . org-roam-dailies-capture-tomorrow)
;; 		 )))


(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  ;;; For inmediate smmothing insertion of a node
  (let ((args (push arg args))
        (org-roam-capture-templates
	 (list
	  (append (car org-roam-capture-templates)
                  '(:immediate-finish t)
		  ))
	 )
	)
    (apply #'org-roam-node-insert args)
   )
)


(defun my/org-roam-filter-by-tag (tag-name)
  ;;; Check if the node has the specified tag
  (lambda (node)
    (member tag-name (org-roam-node-tags node))
  )
)

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list)
	   )
   )
)


(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  ;;; Refresh the agenda list.
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)


(defun my/org-roam-project-finalize-hook ()
  "Add the captured project file to `org-agenda-files' if the capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
 ;;; Add project file to the agenda list if the capture was confirmed
 (unless org-note-abort
   (with-current-buffer (org-capture-get :buffer)
    (add-to-list 'org-agenda-files (buffer-file-name))
   )
 )
)



(defun my/org-roam-find-project()
  "Search for a project"
  (interactive)
  ;;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))



(defun my/org-roam-capture-inbox()
  "The capture functionality for fleeting notes."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))



(defun my/org-roam-capture-inbox-from-directory (directory)
  "The capture functionality for fleeting notes from a DIRECTORY."
  (interactive "sSelect directory: ")
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head (concat buffer-file-name (concat "/" (file-name-as-directory directory) "Inbox.org")) "#+title: Inbox\n")))))



(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))


(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))



(use-package helm-bibtex
     :config
     (setq bibtex-completion-bibliography bib-files-directory
           bibtex-completion-library-path pdf-files-directory
           bibtex-completion-pdf-field "File"
           bibtex-completion-notes-path org-directory
           bibtex-completion-additional-search-fields '(keywords))
     :bind
     (("C-c n B" . helm-bibtex))
)

;;; For html, css and javascript modes.
(use-package web-mode
  :ensure t)

;;; Turn on the emmet minor mode for all `html-mode' and `css-mode' buffers.
(add-hook 'web-mode-hook 'emmet-mode)

;;; Load yasnippet.
(use-package yasnippet
  :ensure t)
(yas-reload-all)
;;; Add it to some programming languages
(add-hook 'prog-mode-hook #'yas-minor-mode)


(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
)


(use-package magit
  :ensure t)


(use-package treemacs
  :ensure t
  :init
  (setq treemacs-follow-after-init t
        treemacs-width 35)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
)


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
)


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
)


(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1)
)

(use-package ag
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (
	 ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
  )
)


(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config
  (treemacs-icons-dired-mode)
)


(global-set-key (kbd "C-x t t") 'treemacs)


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
)


(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure)
)

;;; The erver for IDE functionality
(require 'eglot)
;; Add React, TypeScript, and TSX files to eglot's list of programming modes
(dolist (mode '(typescript-mode web-mode))
  (add-to-list 'eglot-server-programs `(,mode . ("typescript-language-server" "--stdio"))))

;; For TSX files, associate them with web-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; For codeium services.
(add-to-list 'load-path "c:/Users/Asus/AppData/Roaming/.emacs.d/codeium")

;; we recommend using use-package to organize your init.el

(require 'codeium)
(with-eval-after-load 'codeium
  ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
)


;(use-package codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

 ;   :init
    ;; use globally
;;(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

(prefer-coding-system 'utf-8)
;;(server-start)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)))


(use-package org-noter
  :config
  ;; Your org-noter config ........
  (require 'org-noter-pdftools)
)


(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link)
)


(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


(add-hook 'scheme-mode-hook 'geiser-mode)
(setq geiser-default-implementation 'racket)


;; Read ePub files
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)



;; MINIBUFFER COMPLETION

  ;; Enable vertico
  ;; (use-package vertico
  ;;   :init
  ;;   (vertico-mode)
  ;;   :custom
  ;;   (vertico-sort-function 'vertico-sort-history-alpha))

  ;; Persist history over Emacs restarts.
  ;; (use-package savehist
  ;;   :init
  ;;   (savehist-mode))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))
  )
  )

(require 'org-roam-protocol)

(server-start)
(defun my-highlight-wrapped-text ()
  "Highlight text wrapped in ~ characters."
  (font-lock-add-keywords
   nil
   '(("~\\([^~]+\\)~"
      (0 (put-text-property (match-beginning 0) (match-end 0) 'face '(:foreground "red" :weight bold)))))))

(add-hook 'org-mode-hook #'my-highlight-wrapped-text)

;;; For java IDE Configuration
(require 'eglot)

;; Configure eglot to connect to JDTLS running on port 8090
(setq eglot-server-programs
      '((java-mode . (:host "localhost" :port 8090))))


;;; .emacs.el ends here:
