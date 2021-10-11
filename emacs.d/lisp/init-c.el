;;;init-c:
;;; ref:
;;;  https://blog.dowhile0.org/2011/07/04/emacs-configuration-for-linux-kernel-development/
;;;  https://www.emacswiki.org/emacs/CScopeAndEmacs#h5o-2
;;; The Linux kernel coding style is very strict
(require-package 'xcscope)
(setq cscope-do-not-update-database t)

;; make cscope

(setq c-default-style "linux")

(provide 'init-c)
