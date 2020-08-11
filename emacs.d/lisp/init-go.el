;;;init mode for google golang code which
;;;depend on http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;; make sure all those go packages installed or just install as followed:
;;; go get -u github.com/mdempsky/gocode
;;; go get -u golang.org/x/tools/cmd/guru
;;; go get -u github.com/rogpeppe/godef
;;; go get -u golang.org/x/tools/cmd/goimports
;;; go get -u golang.org/x/tools/cmd/gorename
;;;
;;; make sure $PATH include $GOPATH/bin
;;; export PATH=$PATH:$GOPATH/bin


;; update 2020-08-12
;; https://arenzana.org/2019/12/emacs-go-mode-revisited/


;; set GOPATH by run .env shell scripts
(defun get-env (path)
  (cond ((or (string-equal path "/") (not (file-exists-p path))) nil)
        ((file-exists-p (concat path "/.env")) (concat path "/.env"))
        ((file-exists-p (concat path "/env.sh")) (concat path "/env.sh"))
        (t (get-env (file-name-directory (directory-file-name path))))))

(defun set-gopath (path)
  (let ((gopath (get-env path)))
    (unless gopath
      (setq gopath (concat (getenv "HOME") "/.profile")))
    (setenv "GOPATH" (string-trim (shell-command-to-string (concat "$SHELL --login -i -c 'cd " (file-name-directory (directory-file-name gopath)) ";source " gopath "; echo $GOPATH;'"))))))

(defun set-current-gopath ()
  (interactive) 
  (set-gopath (buffer-file-name))
  (message (getenv "GOPATH")))

(defun set-default-gopath ()
  (interactive)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOROOT")
    (exec-path-from-shell-copy-env "GOPATH")))

(set-default-gopath)

;;; 2020-08-11 added
(defun custom-go-mode ()
  (display-line-numbers-mode 1))

(use-package go-mode
:defer t
:ensure t
:mode ("\\.go\\'" . go-mode)
:init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")  
  (setq compilation-read-command nil)
  (add-hook 'go-mode-hook 'custom-go-mode)
:bind (("M-," . compile)
("M-." . godef-jump)))

(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	(let* ((w (split-window-vertically))
	       (h (window-height w)))
	  (select-window w)
	  (switch-to-buffer "*compilation*")
	  (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(setq compilation-scroll-output t)


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;Optional - provides fancier overlays.

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
)

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;;Optional - provides snippet support.

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;;lsp-ui-doc-enable is false because I don't like the popover that shows up on the right
;;I'll change it if I want it back


(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)

(provide 'init-go)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; init-go ends here
