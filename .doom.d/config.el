;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rohan Verma"
      user-mail-address "hello@rohanverma.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Noto Sans Mono" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/sync/org/")
(setq org-roam-directory "~/Nextcloud/sync/org/roam/")

;; hide bold/italics in org files
(setq org-hide-emphasis-markers t)


(setq diary-file "~/Nextcloud/sync/org/diary-file")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; org-mouse
(require 'org-mouse)

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/Nextcloud/sync/org/elfeed.org"))

;; Projectile
(setq projectile-project-search-path '("~/Documents/Zerodha/gitlab/" "~/Documents/Projects/"))

;; Org column
(setq org-tags-column -80)

;; Org Protocol
(require 'org-protocol)

;; https://github.com/sprig/org-capture-extension
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

(setq org-capture-templates `(
                              ("p" "Protocol" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                              ("L" "Protocol Link" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
                               "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
                              ("j" "Journal Entry"
                               item (file+datetree ,(concat org-directory "journal.org"))
                               "%(format-time-string org-journal-time-format)% \n%?"
                               :empty-lines 0)
                              ("t" "Todo Entry"
                               entry (file+headline, (concat org-directory "todo.org") "Inbox")
                               "* %?")
                              ))

;; Org Todo
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-log-done 'time)

;; Org Capture Frame
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; https://fuco1.github.io/2017-09-02-Maximize-the-org-capture-buffer.html
(defvar my-org-capture-before-config nil
  "Window configuration before `org-capture'.")

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  ;; In case we run capture from emacs itself and not an external app,
  ;; we want to restore the old window config
  (when my-org-capture-before-config
    (set-window-configuration my-org-capture-before-config))
  (-when-let ((&alist 'name name) (frame-parameters))
    (when (equal name "org-protocol-capture")
      (delete-frame))))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-cleanup)

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
          'delete-other-windows)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 120)
                (height . 30)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture))

(defun make-calendar-frame ()
  "Create a new frame and run calendar."
  (interactive)
  (make-frame '((name . "calendar")))
  (select-frame-by-name "calendar")
  (=calendar))

;; Nov.el mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Define opening urxvt
(defun myterm ()
  "Spawn a term instance based on `default-directory' of current buffer."
  (interactive)
  (let ((myterm "alacritty"))
    (start-process myterm nil myterm )))

;; Custom key bindings
(bind-key "C-M-t" #'myterm)

;; magit forge
(with-eval-after-load 'forge-core
  (add-to-list 'forge-alist '("gitlab.zerodha.tech" "gitlab.zerodha.tech/api/v4" "gitlab.zerodha.tech" forge-gitlab-repository))
  (add-to-list 'forge-alist '("gitlab.zerodha.tech:2280" "gitlab.zerodha.tech/api/v4" "gitlab.zerodha.tech" forge-gitlab-repository))
  (add-to-list 'auth-sources "~/.authinfo")
  )

;; https://github.com/hlissner/doom-emacs/issues/3038
(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; Deft settings
(setq deft-directory "~/Nextcloud/sync/org/"
      deft-extensions '("org")
      deft-recursive t)

;; use mu4e for e-mail in emacs
(setq mu4e-mu4e-mail-path "~/Mail")
(set-email-account! "rohanvermanet"
                    '((mu4e-sent-folder       . "/Sent")
                      (mu4e-drafts-folder     . "/Drafts")
                      (mu4e-trash-folder      . "/Trash")
                      (mu4e-refile-folder     . "/Archives")
                      (smtpmail-smtp-user     . "hello@rohanverma.net")
                      (user-mail-address      . "hello@rohanverma.net")    ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nRohan Verma"))
                    t)
(setq mu4e-sent-messages-behavior 'sent)

;; hook for vue
(add-hook 'vue-mode-hook #'lsp!)

;; org-super-agenda config
(setq  org-super-agenda-groups '((:name "Today"
                                  :time-grid t
                                  :scheduled today)
                                 (:name "Due today"
                                  :deadline today)
                                 (:name "Important"
                                  :priority "A")
                                 (:name "Overdue"
                                  :deadline past)
                                 (:name "Due soon"
                                  :deadline future)
                                 (:name "Big Outcomes"
                                  :tag "bo")))

(after! org-tree-slide
  (setq org-tree-slide-skip-outline-level 1))

;; golangci-lint
(setq flycheck-golangci-lint-config "~/Documents/Zerodha/gitlab/commons/gitlab-templates/golang/.golangci.yml")
