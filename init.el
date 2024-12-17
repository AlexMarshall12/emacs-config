;;(setq debug-on-error f)
(setq inhibit-startup-messages t)

;; Modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(pixel-scroll-precision-mode 1)

;; ALL PACKAGE STUFF
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")) ;
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(load-theme 'solarized-light t)

(use-package ace-window
  :bind ("C-x o" . ace-window))
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'ace-window)
(defun find-file-ace-window ()
  "Use ace window to select a window for opening a file from dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (> (length (aw-window-list)) 1)
        (aw-select "" (lambda (window)
                        (aw-switch-to-window window)
                        (find-file file)))
      (find-file-other-window file))))

(setq compilation-ask-about-save nil)
(setq dired-dwim-target t)

(use-package consult
  :ensure t
  :bind (
 ("C-c M-x" . consult-mode-command)
 ("C-c h" . consult-history)
 ("C-s" . consult-line)
 ("M-s r" . consult-ripgrep))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; Enabled right away. Note that this forces loading the package.
  (marginalia-mode))
(put 'dired-find-alternate-file 'disabled nil)

(use-package god-mode
  :ensure t
  :init
  (god-mode))

(global-set-key (kbd "<CapsLock>") #'god-local-mode)


;;(use-package meow
;;  :ensure t
;;  :config
;;  ;; Enable meow-mode globally
;;  (meow-global-mode 1)
;;  
;;  ;; Define your custom keybindings and settings here
;;
;;(defun meow-setup ()
;;  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;  (meow-motion-overwrite-define-key
;;   '("j" . meow-next)
;;   '("k" . meow-prev)
;;   '("<escape>" . ignore))
;;  (meow-leader-define-key
;;   ;; SPC j/k will run the original command in MOTION state.
;;   '("j" . "H-j")
;;   '("k" . "H-k")
;;   ;; Use SPC (0-9) for digit arguments.
;;   '("1" . meow-digit-argument)
;;   '("2" . meow-digit-argument)
;;   '("3" . meow-digit-argument)
;;   '("4" . meow-digit-argument)
;;   '("5" . meow-digit-argument)
;;   '("6" . meow-digit-argument)
;;   '("7" . meow-digit-argument)
;;   '("8" . meow-digit-argument)
;;   '("9" . meow-digit-argument)
;;   '("0" . meow-digit-argument)
;;   '("/" . meow-keypad-describe-key)
;;   '("?" . meow-cheatsheet))
;;  (meow-normal-define-key
;;   '("0" . meow-expand-0)
;;   '("9" . meow-expand-9)
;;   '("8" . meow-expand-8)
;;   '("7" . meow-expand-7)
;;   '("6" . meow-expand-6)
;;   '("5" . meow-expand-5)
;;   '("4" . meow-expand-4)
;;   '("3" . meow-expand-3)
;;   '("2" . meow-expand-2)
;;   '("1" . meow-expand-1)
;;   '("-" . negative-argument)
;;   '(";" . meow-reverse)
;;   '("," . meow-inner-of-thing)
;;   '("." . meow-bounds-of-thing)
;;   '("[" . meow-beginning-of-thing)
;;   '("]" . meow-end-of-thing)
;;   '("a" . meow-append)
;;   '("A" . meow-open-below)
;;   '("b" . meow-back-word)
;;   '("B" . meow-back-symbol)
;;   '("c" . meow-change)
;;   '("d" . meow-delete)
;;   '("D" . meow-backward-delete)
;;   '("e" . meow-next-word)
;;   '("E" . meow-next-symbol)
;;   '("f" . meow-find)
;;   '("g" . meow-cancel-selection)
;;   '("G" . meow-grab)
;;   '("h" . meow-left)
;;   '("H" . meow-left-expand)
;;   '("i" . meow-insert)
;;   '("I" . meow-open-above)
;;   '("j" . meow-next)
;;   '("J" . meow-next-expand)
;;   '("k" . meow-prev)
;;   '("K" . meow-prev-expand)
;;   '("l" . meow-right)
;;   '("L" . meow-right-expand)
;;   '("m" . meow-join)
;;   '("n" . meow-search)
;;   '("o" . meow-block)
;;   '("O" . meow-to-block)
;;   '("p" . meow-yank)
;;   '("q" . meow-quit)
;;   '("Q" . meow-goto-line)
;;   '("r" . meow-replace)
;;   '("R" . meow-swap-grab)
;;   '("s" . meow-kill)
;;   '("t" . meow-till)
;;   '("u" . meow-undo)
;;   '("U" . meow-undo-in-selection)
;;   '("v" . meow-visit)
;;   '("w" . meow-mark-word)
;;   '("W" . meow-mark-symbol)
;;   '("x" . meow-line)
;;   '("X" . meow-goto-line)
;;   '("y" . meow-save)
;;   '("Y" . meow-sync-grab)
;;   '("z" . meow-pop-selection)
;;   '("'" . repeat)
;;   '("<escape>" . ignore)))
;;  
;;  (meow-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "Alexs-MacBook-Pro.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(package-selected-packages
   '(god-mode boon meow exec-path-from-shell dired-subtree treemacs-magit treemacs-tab-bar evil-collection orderless org-journal projectile ibuffer-vc evil-leader evil-commentary solarized-theme dumb-jump marginalia swiper-helm perspective ivy-rich use-package all-the-icons counsel pyvenv vterm treemacs-evil consult treemacs-icons-dired vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
