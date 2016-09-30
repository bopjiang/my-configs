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

;; hide toolbar
(tool-bar-mode -1)

;; hide menu
(menu-bar-mode -1)

;; hide scroll bar
(scroll-bar-mode -1)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(setenv "GOPATH"   (concat (getenv "HOME") "ws"))

;; normally, I put all the go tools in default PATH; /usr/local/bin/
;; go get github.com/nsf/gocode; mv $GOPATH/bin/gocode /usr/local/bin/



;; gofmt
;; ===aleady called in godef
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; godef
(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; Now you can jump into code with M-. and jump back with M-*

;; Autocomplete
;; Install melpa auto-complete via M-x package-install followed by auto-complete
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

;;Install the melpa package via M-x package-install followed by go-autocomplete
;;(defun auto-complete-for-go ()
;;(auto-complete-mode 1))
;;(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))



(require 'auto-complete-config)
(ac-config-default)


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

