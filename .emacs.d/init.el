;;  ;; Define and initialise package repositories
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (package-initialize)

;; ;; use-package to simplify the config file
;; (unless (package-installed-p 'use-package)
;;    (package-refresh-contents)
;;    (package-install 'use-package))
;; (require 'use-package)
;; (setq use-package-always-ensure 't)

;;  ;; Theme
;; (use-package exotica-theme
;;   :config (load-theme 'exotica t))

;;   ;; Keyboard-centric user interface
;;   ;;(setq inhibit-startup-message t)
;;   (tool-bar-mode -1)
;;   (menu-bar-mode -1)
;;   (scroll-bar-mode -1)
;; (defalias 'yes-or-no-p 'y-or-n-p)

;;   ;; Define the init file
;;   (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;   (when (file-exists-p custom-file)
;;     (load custom-file))

;; (setq cursor-type 'bar)

;; (setq ob-mermaid-cli-path "C:/Users/Asus/AppData/Roaming/npm/mmdc.exe")

;; (org-babel-do-load-languages
;;     'org-babel-load-languages
;;     '((mermaid . t)
;;       (scheme . t)
;;       ))

;; ;; Helm configuration
;;   (use-package helm
;;     :config
;;     (require 'helm-config)
;;     :init
;;     (helm-mode 1)
;;     :bind
;;     (("M-x"     . helm-M-x) ;; Evaluate functions
;;      ("C-x C-f" . helm-find-files) ;; Open or create files
;;      ("C-x b"   . helm-mini) ;; Select buffers
;;      ("C-x C-r" . helm-recentf) ;; Select recently saved files
;;      ("C-c i"   . helm-imenu) ;; Select document heading
;;      ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
;;      :map helm-map
;;      ("C-z" . helm-select-action)
;;      ("<tab>" . helm-execute-persistent-action)))


;;  (use-package which-key
;;     :config
;;     (which-key-mode)
;;     (setq which-key-idle-delay 0.5
;;           which-key-idle-secondary-delay 0.5)
;;     (which-key-setup-side-window-bottom))

;; ;; Auto completion
;;   (use-package company
;;     :config
;;     (setq company-idle-delay 0
;;           company-minimum-prefix-length 4
;;           company-selection-wrap-around t))
;; (global-company-mode)

;;  ;; Set default, fixed and variabel pitch fonts
;;   ;; Use M-x menu-set-font to view available fonts
;;   (use-package mixed-pitch
;;     :hook
;;     (text-mode . mixed-pitch-mode)
;;     ;;:config
;;     ;;(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
;;     ;;(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
;;     ;;(set-face-attribute 'variable-pitch nil :font "DejaVu Sans")
;;     )
;; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

;;   ;; Required for proportional font
;;   (use-package company-posframe
;;     :config
;;     (company-posframe-mode 1))

;;  ;; Improve org mode looks
;;     (setq org-startup-indented t
;;           org-pretty-entities t
;;           org-hide-emphasis-markers t
;;           org-startup-with-inline-images t
;;           org-image-actual-width '(300))

;;  ;; Show hidden emphasis markers
;;   (use-package org-appear
;;     :hook (org-mode . org-appear-mode))

;; ;; Nice bullets
;;   (use-package org-superstar
;;       :config
;;       (setq org-superstar-special-todo-items t)
;;       (add-hook 'org-mode-hook (lambda ()
;;                                  (org-superstar-mode 1))))

;;  ;; Increase size of LaTeX fragment previews
;; (plist-put org-format-latex-options :scale 2)

;; ;; Increase line spacing
;; (setq-default line-spacing 6)

;; ;; Distraction-free screen
;;   (use-package olivetti
;;     :init
;;     (setq olivetti-body-width .67)
;;     :config
;;     (defun distraction-free ()
;;       "Distraction-free writing environment"
;;       (interactive)
;;       (if (equal olivetti-mode nil)
;;           (progn
;;             (window-configuration-to-register 1)
;;             (delete-other-windows)
;;             (text-scale-increase 2)
;;             (olivetti-mode t))
;;         (progn
;;           (jump-to-register 1)
;;           (olivetti-mode 0)
;;           (text-scale-decrease 2))))
;;     :bind
;;     (("<f9>" . distraction-free)))

;;   ;; Sensible line breaking
;;   (add-hook 'text-mode-hook 'visual-line-mode)
  
;;   ;; Overwrite selected text
;;   (delete-selection-mode t)
  
;;   ;; Scroll to the first and last line of the buffer
;; (setq scroll-error-top-bottom t)

;;  ;; Org-Mode initial setup
;;   (use-package org
;;     :bind
;;     (("C-c l" . org-store-link)
;;      ("C-c a" . org-agenda)
;;      ("C-c c" . org-capture)))

;; ;; Org-Roam basic configuration
;;   (setq org-directory (concat (getenv "HOME") "/Documents/notes/"))

;;   (use-package org-roam
;;     :after org
;;     :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
;;     :custom
;;     (org-roam-directory (file-truename org-directory))
;;     :config
;;     (org-roam-setup)
;;     :bind (("C-c n f" . org-roam-node-find)
;;            ("C-c n r" . org-roam-node-random)		    
;;            (:map org-mode-map
;;                  (("C-c n i" . org-roam-node-insert)
;;                   ("C-c n o" . org-id-get-create)
;;                   ("C-c n t" . org-roam-tag-add)
;;                   ("C-c n a" . org-roam-alias-add)
;;                   ("C-c n l" . org-roam-buffer-toggle)))))
;;  (use-package helm-bibtex
;;     :config
;;     (setq bibtex-completion-bibliography bib-files-directory
;;           bibtex-completion-library-path pdf-files-directory
;;           bibtex-completion-pdf-field "File"
;;           bibtex-completion-notes-path org-directory
;;           bibtex-completion-additional-search-fields '(keywords))
;;     :bind
;;     (("C-c n B" . helm-bibtex)))

;; ;; Configure eglot using use-package
;; (use-package eglot
;;   :ensure t
;;   :hook (prog-mode . eglot-ensure))

;; ;; Add React, TypeScript, and TSX files to eglot's list of programming modes
;; (dolist (mode '(typescript-mode web-mode))
;;   (add-to-list 'eglot-server-programs `(,mode . ("typescript-language-server" "--stdio"))))

;; ;; For TSX files, associate them with web-mode
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((scheme . t)))

;; (add-hook 'scheme-mode-hook 'geiser-mode)
;; (setq geiser-default-implementation 'racket)
