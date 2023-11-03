;; [[file:init.org::*Before init][Before init:1]]
;; (setq org-export-async-debug nil)
(setenv "LANG" "pt_BR.utf-8")
;; (setenv "LOCALE" "utf-8")

(setq inhibit-startup-screen             t
      confirm-kill-processes           nil
      gc-cons-threshold          500000000)
(put 'narrow-to-region       'disabled nil)
(put 'downcase-region        'disabled nil)

;; no more yes-or-no (tks larstvei)
(fset 'yes-or-no-p 'y-or-n-p)
;; Before init:1 ends here

;; [[file:init.org::*Package Management][Package Management:1]]
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'dash)
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)
(require 'bind-key)

;; auto-package-update
(use-package auto-package-update
  :config
  (auto-package-update-maybe)
  :custom
  (auto-package-update-delete-old-versions t))
;; Package Management:1 ends here

;; [[file:init.org::*Load Settings][Load Settings:1]]
(add-to-list 'load-path (concat user-emacs-directory "init-files-c"))
(add-to-list 'load-path "~/.emacs.d/init-files/my-packages")
(require '~cond~>)

(require 'org-settings)
(require 'org-pretty)
(require 'tools)
(require 'languages)
(require 'visuals)

(if (string= system-type "gnu/linux")
    (load "linux.el")
  (load "macos.el"))

(when (file-exists-p "./init-files/secrets.org")
  (require 'secrets))
;; Load Settings:1 ends here

;; [[file:init.org::*Emacs VCS][Emacs VCS:1]]
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq delete-old-versions t
      kept-old-versions 2
      version-control t)
;; Emacs VCS:1 ends here

;; [[file:init.org::*Helper Functions][Helper Functions:1]]
(defun k/tangle-inits ()
  "this function is a recurring sntippet online, this is my spin on it
  if the current buffer is '.org', and in '~/.emacs.d the code-blocks are
  tangled. (someday the tangled file will be compiled, but not today)"
  ;; avoid running hooks when tangling.
  (-let* ((prog-mode-hook nil)
	        (this-file (buffer-file-name)))
    ;; (target-file (s-replace ".org" ".el" this-file)))
    (when (and (string-suffix-p ".org" this-file)
		           (or (string-prefix-p (expand-file-name user-emacs-directory)
				                            this-file)
                   (string-prefix-p (expand-file-name "~/.stumpwm.d")
                                    this-file)))
	    (org-babel-tangle)
	    )))

(defun k/tangle-stumpwm-inits ()
  (interactive)
  (require 'org)
  (async-start
   (lambda ()
     (dolist (file (--filter (string-suffix-p ".org" it)
			                       (directory-files "~/.stumpwm.d/" t)))
	     (find-file file)
	     (org-babel-tangle)))))

(defun k/tangle-all-inits ()
  (interactive)
  (require 'org)
  (async-start
   (lambda ()
     (dolist (file (--filter (string-suffix-p ".org" it)
			                       (directory-files "~/.emacs.d/init-files/" t)))
	     (find-file file)
	     (org-babel-tangle)))))

(add-hook 'after-save-hook 'k/tangle-stumpwm-inits)
(add-hook 'kill-emacs-hook 'k/tangle-stumpwm-inits)

(add-hook 'after-save-hook 'k/tangle-inits)
(add-hook 'kill-emacs-hook 'k/tangle-all-inits)
;; Helper Functions:1 ends here

;; [[file:init.org::*Helper Functions][Helper Functions:2]]
(setq k/remove-from-find-inits-list '("org-async.org"))

(setq k/find-inits-list
      (-concat '("init.org")
               (-difference
                (--filter (string-suffix-p ".org" it)
                          (directory-files "~/.emacs.d/init-files/" ))
                k/remove-from-find-inits-list)))

(defun k/find-inits (file)
  "A lovely trick to learn on emacs. FILE is read with completion,
it's possibilities are read from the directory with my settings"
  (interactive
   (list (completing-read
          "Init File: "
          k/find-inits-list)))
  (if (string-equal file "init.org")
      (find-file (expand-file-name "~/.emacs.d/init.org"))
    (find-file (expand-file-name (concat "~/.emacs.d/init-files/"
                                         file)))))
;; Helper Functions:2 ends here

;; [[file:init.org::*Helper Functions][Helper Functions:3]]
(defun k/prepare-init (&rest _)
  "Just how i like my emacs startup screen."
  (find-file (expand-file-name "~/Documents/workspace/notas/todo.org")))

(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)))

(add-hook 'after-init-hook 'k/prepare-init)
;; Helper Functions:3 ends here

;; [[file:init.org::*Reset GC][Reset GC:1]]
(setq gc-cons-threshold   (* 2 1000 1000))
;; Reset GC:1 ends here

;; [[file:init.org::*General kbds][General kbds:1]]
(global-set-key (kbd "M-w")    'widen)
(global-set-key (kbd "C-d")    'eval-defun)
(global-set-key (kbd "M-r")    'eval-region)
(global-set-key (kbd "M-i")    'k/find-inits)
(global-set-key (kbd "M-?")    'eval-last-sexp)
(global-set-key (kbd "M-n")    'narrow-to-region)
(global-set-key (kbd "<down>") 'next-logical-line)
(global-set-key (kbd "<up>")   'previous-logical-line)
(global-set-key (kbd "C-c b")  'switch-to-buffer-other-window)
(global-set-key (kbd "C-x f")  'find-file)
(global-set-key (kbd "M-<backspace>")  'backward-kill-word)
;; General kbds:1 ends here
