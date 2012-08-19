(add-to-list 'load-path "~/.emacs.d")

;;;; Full sreen on startup
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(global-set-key [f11] 'toggle-fullscreen)

(toggle-fullscreen)

;;;; recent file history
(require 'recentf)
(recentf-mode 1)

;;;; font config 字体配置
(set-default-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1")
;; GNU Emacs 23 has a built-in key combination:
;; `C-x C-+’ and ‘C-x C--’ to increase or decrease the buffer text size

;;;; theme config 
(require 'color-theme) 
(color-theme-dark-laptop)
;;(color-theme-initialize)
;;(color-theme-tty-dark)
;;(color-theme-comidia)

;;;;  file coding utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal


;;;; graphviz
;; C-c c 编译dot文件
;; C-c p 预览生成的图片
(load-file "~/.emacs.d/graphviz-dot-mode.el")


;;;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; set the variable org-agenda-files so that org-mode will know which files to
;; search for TODOs and scheduled items.
(setq org-agenda-files (list "~/org/tech.org"
                             "~/org/home.org"
                             "~/org/invest.org"))

;;;; Erlang
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.7/emacs/" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;;; Distel for erlang
(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

;;;cscope, for reading C/C++ code 
(load-file "~/.emacs.d/xcscope.el")
(require 'xcscope)
