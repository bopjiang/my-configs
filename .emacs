;============================= General Setting ==================================

(add-to-list 'load-path "~/.emacs.d")
;; emacs的模块名称必须同文件名一致，同java类似

;; recover buffers opening when startup
(desktop-save-mode 1)

;; share clipboard with system
;; 系统剪贴板快捷键（C-c C-c复制，C-c C-v粘贴）
(global-set-key "\C-c\C-c" 'clipboard-kill-ring-save)
(global-set-key "\C-c\C-v" 'clipboard-yank)

;; C-Space被输入法占用，改用C-c m来标记文本块
(global-set-key "\C-cm" 'set-mark-command)

;; 关闭toolbar
(tool-bar-mode)

;; 窗口操作
(require 'window-numbering)
(window-numbering-mode 1)

;; deal with trail white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 显示总行号
;; display the total number of lines in the Emacs modeline
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
                         (let ((str "L%l"))
                           (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                             (setq str (concat str "/" my-mode-line-buffer-line-count)))
                           str)))
                "  %p"
                (list 'column-number-mode "  C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))

(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

;; 用ibuffer代替默认的buffer switch
;; 参考 http://www.emacswiki.org/emacs/IbufferMode
;; ibuffer是emacs自带的，可用 M-x ibuffer 调出来
;; 下面只是将快捷键 C-x C-b 映射为调出ibuffer的命令
;; 在ibuffer中的操作方式和dired mode一样
;; n和p是上下，m是选中该文件，可选多个，D是kill buffer
;; 回车或者按e(就是edit)来编辑文件
;; 在ibuffer页面，用英文斜线来通过关键字过滤缩小显示文件范围
;; 比如 “ /日记 ” ，就会只显示buffer名称中有日记这两个字的
;; / 后面支持正则表达式 如 /200?
;; 撤销过滤按两下/，就是按 “ // ”
;; 在ibuffer中，按英文等号 “ = ” 对为保存文件和它上一个保存版本做diff
;; 按 g 刷新文件目录
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

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

;;(toggle-fullscreen)

;;打开文件时自动启用 hideshow (hs-minor-mode)
;; hide-show
(setq hs-allow-nesting t)

;; show-paren-mode allows one to see matching pairs of parentheses and other characters.
(show-paren-mode 1)
(setq show-paren-delay 0)

(add-hook 'c-mode-hook
	  '(lambda ()
	     (hide-ifdef-mode t)
	     (semantic-default-c-setup)
	     (hs-minor-mode t)
             (define-key hs-minor-mode-map [f12] 'hs-toggle-hiding)))

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (hs-minor-mode t)
	    (define-key hs-minor-mode-map [f12] 'hs-toggle-hiding)))

;; not working here
;(define-key hs-minor-mode-map [\C-h ?H] 'hs-hide-all)
;(define-key hs-minor-mode-map [?/C-h ?S] 'hs-show-all)
;(define-key hs-minor-mode-map [?/C-h ?h] 'hs-hide-block)
;(define-key hs-minor-mode-map [?/C-h ?s] 'hs-show-block)
;(define-key hs-minor-mode-map [?/C-h ?l] 'hs-hide-level)
;(define-key hs-minor-mode-map [f12] 'hs-toggle-hiding)

;;;; recent file history
(require 'recentf)
(recentf-mode 1)

;;;; font config 字体配置
(set-default-font "-unknown-文泉驿等宽微米黑-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1")
;; GNU Emacs 23 has a built-in key combination:
;; `C-x C-+’ and ‘C-x C--’ to increase or decrease the buffer text size

;;;; theme config
(require 'color-theme)
(setq color-theme-is-global t)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;;     (color-theme-dark-laptop)
;;       (color-theme-hober)
;;     (color-theme-tty-dark)
;;     (color-theme-comidia)
   ))


;;;;  file coding utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

;;;; Edit
;(define-key   global-map   "\C-M-g "   'goto-line)
(global-set-key   [?\C-\M-g]   'goto-line)

;;; smart complete
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

; change location of the template files
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;============================= Tools ============================================

;----------------------------- graphviz --------------------------
;; C-c c 编译dot文件
;; C-c p 预览生成的图片
(load-file "~/.emacs.d/graphviz-dot-mode.el")

;----------------------------- org-mode --------------------------
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
(setq org-mobile-directory "~/Dropbox/orgmode")

;----------------------------- erlang --------------------------
;;;; Erlang
;(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.7/emacs/" load-path))
;(setq erlang-root-dir "/usr/local/lib/erlang")
;(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;(require 'erlang-start)

;;; Distel for erlang
;(add-to-list 'load-path "/usr/local/share/distel/elisp")
;(require 'distel)
;(distel-setup)

;----------------------------- C C++ --------------------------
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

(defun my-c-mode-common-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  ;;; 设置缩进为4
  (setq c-basic-offset 4)
  ;; (c-set-style "linux")
  ;;; hungry-delete and auto-newline
  ;; (c-toggle-auto-hungry-state 1)
  ;;按键定义
  ;;(define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  ;;(define-key c-mode-base-map [(f7)] 'compile)
  (define-key c-mode-base-map [(meta \`)] 'c-indent-command)
  ;; (define-key c-mode-base-map [(tab)] 'hippie-expand)
  ;; (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;;预处理设置
  ;;(setq c-macro-shrink-window-flag t)
  ;;(setq c-macro-preprocessor "cpp")
  ;;(setq c-macro-cppflags " ")
  ;;(setq c-macro-prompt-flag t)
  ;;(setq hs-minor-mode t)
  ;;(setq abbrev-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;;我的C++语言编辑策略
(defun my-c++-mode-hook()
  (setq tab-width 4 indent-tabs-mode nil)
  (setq c-basic-offset 4)
;;  (define-key c++-mode-map [f3] 'replace-regexp)
)

(add-hook 'c++-mode-common-hook 'my-c++-mode-common-hook)

;;;; CSCOPE, for reading C/C++ code
(load-file "~/.emacs.d/xcscope.el")
(require 'xcscope)

;;;; CEDIT

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another
;; package (Gnus, auth-source, ...).
(load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
(semantic-load-enable-gaudy-code-helpers)


;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)
(add-to-list 'load-path "~/.emacs.d/plugins/ecb-2.40")
(require 'ecb)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/src/agile_sd/Payroll/PayrollCode/"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;; CEDET中还有一个不错的工具是speedbar，你可以用它在多个文件中快速切换
;;(global-set-key [(f4)] 'speedbar)

;;; Smart Compile
(require 'smart-compile)
(global-set-key [f9] 'smart-compile)


(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/quick-jump"))
(require 'quick-jump)
(quick-jump-default-keybinding)


;;;; auto-completex
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

(local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
(local-set-key "." 'semantic-complete-self-insert)
(local-set-key ">" 'semantic-complete-self-insert)


