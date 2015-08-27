;============================= General Setting ==================================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

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

;; global mark ring
(global-set-key "\C-x\C-p" 'pop-global-mark)

;; winner-mode
(winner-mode 1)

;; 关闭toolbar
(tool-bar-mode -1)

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

;; Disable mail function
(global-set-key "\C-xm" 'execute-extended-command)

;;打开文件时自动启用 hideshow (hs-minor-mode)
;; hide-show
(setq hs-allow-nesting t)

;; show-paren-mode allows one to see matching pairs of parentheses and other characters.
(show-paren-mode 1)
(setq show-paren-delay 0)

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
         (color-theme-tty-dark)
;;       (color-theme-comidia)
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
(global-set-key   [?\M-g]   'goto-line)

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

;;;; CSCOPE, for reading C/C++ code
(load-file "~/.emacs.d/xcscope.el")
(require 'xcscope)

;;;; auto-completex
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

(local-set-key (kbd "M-/") 'semantic-complete-analyze-inline)
(local-set-key "." 'semantic-complete-self-insert)
(local-set-key ">" 'semantic-complete-self-insert)


;----------------------------- Golang --------------------------
(require 'go-mode-load)
(require 'go-autocomplete)

;;;;; speedbar
;(speedbar 1)
;(when window-system          ; start speedbar if we're using a window system
;    (speedbar t))

;(speedbar-add-supported-extension ".go")

(add-hook
 'go-mode-hook
 '(lambda ()
    ;; hide show mode
    (hs-minor-mode t)
    (define-key hs-minor-mode-map [f12] 'hs-toggle-hiding)
    ;; gocode
    (auto-complete-mode 1)
    (setq ac-sources '(ac-source-go))
    ;; Imenu & Speedbar
    (setq imenu-generic-expression
	  '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
	    ("func" "^func *\\(.*\\) {" 1)))
    (imenu-add-to-menubar "Index")
    ;; Outline mode
    (make-local-variable 'outline-regexp)
    (setq outline-regexp "//\\.\\|//[^\r\n\f][^\r\n\f]\\|pack\\|func\\|impo\\|cons\\|var.\\|type\\|\t\t*....")
    (outline-minor-mode 1)
    (local-set-key "\M-a" 'outline-previous-visible-heading)
    (local-set-key "\M-e" 'outline-next-visible-heading)
    ;; Menu bar
    (require 'easymenu)
    (defconst go-hooked-menu
      '("Go tools"
        ["Go run buffer" go t]
        ["Go reformat buffer" go-fmt-buffer t]
        ["Go check buffer" go-fix-buffer t]))
    (easy-menu-define
      go-added-menu
      (current-local-map)
      "Go tools"
      go-hooked-menu)

    ;; Other
    (setq show-trailing-whitespace t)
    ))

;; helper function
(defun go ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

;; helper function
(defun go-fmt-buffer ()
  "run gofmt on current buffer"
  (interactive)
  (if buffer-read-only
      (progn
        (ding)
        (message "Buffer is read only"))
    (let ((p (line-number-at-pos))
	  (filename (buffer-file-name))
	  (old-max-mini-window-height max-mini-window-height))
      (show-all)
      (if (get-buffer "*Go Reformat Errors*")
	  (progn
	    (delete-windows-on "*Go Reformat Errors*")
	    (kill-buffer "*Go Reformat Errors*")))
      (setq max-mini-window-height 1)
      (if (= 0 (shell-command-on-region (point-min) (point-max) "gofmt" "*Go Reformat Output*" nil "*Go Reformat Errors*" t))
	  (progn
	    (erase-buffer)
	    (insert-buffer-substring "*Go Reformat Output*")
	    (goto-char (point-min))
	    (forward-line (1- p)))
	(with-current-buffer "*Go Reformat Errors*"
	  (progn
	    (goto-char (point-min))
	    (while (re-search-forward "<standard input>" nil t)
	      (replace-match filename))
	    (goto-char (point-min))
	    (compilation-mode))))
      (setq max-mini-window-height old-max-mini-window-height)
      (delete-windows-on "*Go Reformat Output*")
      (kill-buffer "*Go Reformat Output*"))))

;; helper function
(defun go-fix-buffer ()
  "run gofix on current buffer"
  (interactive)
  (show-all)
  (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))

;;
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


;---------------------------------markdown mode--------------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(put 'upcase-region 'disabled nil)
