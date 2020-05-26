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
(setq doom-font (font-spec :family "Roboto Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/sync/org/")

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

;; Org Protocol
(require 'org-protocol)

;; https://github.com/sprig/org-capture-extension
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
  (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )

(setq org-capture-templates `(
  ("p" "Protocol" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
        "* TODO %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
  ("L" "Protocol Link" entry (file+headline ,(concat org-directory "todo.org") "Inbox")
        "* TODO %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
  ("j" "Journal Entry"
        item (file+datetree ,(concat org-directory "journal.org"))
         " [%<%Y-%m-%d %H:%M>] %?"
         :empty-lines 0)
  ("t" "Todo Entry"
        entry (file+headline, (concat org-directory "todo.org") "Inbox")
        "* TODO %?")
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

 ;; make the frame contain a single window. by default org-capture
 ;; splits the window.
 (add-hook 'org-capture-mode-hook
           'delete-other-windows)

 (defun make-capture-frame ()
   "Create a new frame and run org-capture."
   (interactive)
   (make-frame '((name . "capture")
                 (width . 120)
                 (height . 15)))
   (select-frame-by-name "capture")
   (setq word-wrap 1)
   (setq truncate-lines nil)
   (org-capture)) 


;; Nov.el mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Define opening urxvt
(defun urxvt ()
  "Spawn a urxvt instance based on `default-directory' of current buffer."
  (interactive)
  (let ((urxvt "urxvt"))
    (start-process urxvt nil urxvt "-cd" (expand-file-name "./"))))

;; Custom key bindings
(bind-key "C-M-t" #'urxvt)
