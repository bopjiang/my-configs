(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;; ======================================================
;;                  UI Configure
;; ======================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; font
(add-to-list 'default-frame-alist
                       '(font . "Monaco-14"))

(menu-bar-mode 1) ;; hide menu
(tool-bar-mode -1)                  ; Disable the button bar atop screen
(scroll-bar-mode -1)                ; Disable scroll bar
(setq inhibit-startup-screen t)     ; Disable startup screen with graphics
;; (setq-default indent-tabs-mode nil) ; Use spaces instead of tabs; ;;oh shit config!
(setq tab-width 2)                  ; Four spaces is a tab
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

;; show line number on left side
; (global-linum-mode t)   ;; no need for most case

;; show column-number
(column-number-mode 1)

;; show time
(display-time)


;; disable startup screen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )

(desktop-save-mode 1)

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


;; ======================================================
;;                  backup setting
;; ======================================================
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; ======================================================
;;                  Go Development Enviornment Configure
;; ======================================================

;; set go exe path
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


;; set go path
(setenv "GOROOT"   "/data/tools/go/")
(setenv "GOPATH"   (concat (getenv "HOME") "/ws"))

;; normally, I put all the go tools in default PATH; /usr/local/bin/
;; go get github.com/nsf/gocode; mv $GOPATH/bin/gocode /usr/local/bin/



;; gofmt
;; ===aleady called in godef
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  
  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'go-guru-definition) ; Go to definition
  (local-set-key (kbd "M-,") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
	
  ;; Misc go stuff
  (auto-complete-mode 1))                         ; Enable auto-complete mode

(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; https://johnsogg.github.io/emacs-golang
;; 2017-12-05

;; go-mode
;; exec-path-from-shell
;; auto-complete
;; go-autocomplete
;; flymake-go
;; neotree
;; atom-one-dark-theme
;;(add-to-list 'load-path "/directGory/containing/neotree/")
(require 'neotree)


;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

;; ======================================================
;;                 org mode
;; ======================================================
;; The following lines are always needed. Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-support-shift-select 'always)


;; ======================================================
;;                Emacs server config
;; ======================================================
;; using systemd
;; https://www.emacswiki.org/emacs/EmacsAsDaemon#toc8

(put 'upcase-region 'disabled nil)


;; =====================================================
;;  clojure
;; =====================================================
(unless (package-installed-p 'cider)
  (package-install 'cider))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

