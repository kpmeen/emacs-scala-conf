(x-focus-frame nil)
(tool-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5955105caeda7430a815c2ca4811a7f7718e3dc27dc3ddf4586c27e4162df7b5" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Configure for Norwegian MacBook Pro keybard layout
(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-left-command-modifier 'super
      mac-right-command-modifier 'meta
      mac-allow-anti-aliasing t
      x-select-enable-clipboard t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; List the packages I want
(setq package-list
  '(ace-jump-mode
    color-theme
    color-theme-solarized
    ensime
    expand-region
    find-file-in-repository
    key-chord
    magit
    markdown-mode
    neotree
    powerline
    ;;rainbow-delimiters
    slime
    theme-looper
    tidy
    yasnippet
    yafolding
    window-numbering))

(package-initialize)

;; Fetch list of packages available
(unless package-archive-contents
	(package-refresh-contents))

;; install the packages that are missing, if any
(dolist (package package-list)
	(unless (package-installed-p package)
		(package-install package)))

;; Use a proper font!
(set-frame-font "Hack-16" nil t)

;; Powerline mode lines
(require 'powerline)
(powerline-default-theme)

(global-linum-mode 1)
;; Always pick up the most recent file from the filesystem
(global-auto-revert-mode 1)

;; Some general-purpose key bindings
(global-set-key (kbd "S-z") 'undo)
(global-set-key (kbd "C-l") 'goto-line) ; [Ctrl]-l]

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".scala" ".sbt" ".sc" ".org" ".txt" ".xml" ".el" ".ini" ".cfg" ".emacs"))

;; Markdown support
(add-hook 'markdown-mode-hook
  (lambda () (electric-indent-local-mode -1)))

;; Neotree navigation
(require 'neotree)
(setq neo-window-width 35)
(global-set-key (kbd "s-d") 'neotree-toggle)
(define-key neotree-mode-map (kbd "i") #'neotree-enter-vertical-split)
(define-key neotree-mode-map (kbd "I") #'neotree-enter-horizontal-split)
(neotree)

(require 'find-file-in-repository)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
 t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
 t)

(require 'yasnippet)
(yas-global-mode 1)

(require 'window-numbering)
(window-numbering-mode 1)

;;(require 'rainbow-delimiters)
;;(add-hook 'scala-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; Exit emacs w/o prompts
(require 'cl)
(defadvice save-buffers-kill-emacs
  (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "S-s") 'save-all)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Put temporary and backup files elsewhere
(setq auto-save-file-name-transforms
  `((".*" ,(concat user-emacs-directory "auto-save/") t)))
(setq backup-directory-alist
  `(("." . ,(expand-file-name
     (concat user-emacs-directory "backups")))))
(setq create-lockfiles nil)

(setq show-paren-delay 0)
(show-paren-mode 1)

(setq server-socket-dir "~/.emacs.d/server")

;; Color theme stuff...
(require 'color-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'Hepatica t)

;; Scala support
(require 'ensime)
(setq ensime-sem-high-faces '(
  (var . (:foreground "#FCE3E5"))
  (val . (:italic :foreground "#FF75E2"))
  (varField . (:foreground "#FCE3E5"))
  (valField . (:foreground "#FF75E2"))
  (functionCall . (:bold :foreground "#A166FF"))
  (operator . (:foreground "#FFFF00"))
  (param . (:foreground "##EFF3FF"))
  (class . (:foreground "#FFFFFF"))
  (trait . (:italic :foreground "#FFFFFF"))
  (object . (:bold :foreground "#FFFFFF"))
  (package . (:foreground "#FFFFFF"))
  (deprecated . (:strike-through "dark gray"))
  ;;(implicitConversion . (:underline (:style wave :color "blue"))
  ;;(implicitParams . (:underline (:style wave :color "blue"))
))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Navigate to Test implementation
(defun jump-to-test ()
  "Jump to correspnding test file"
  (interactive)
  (find-file-other-window
    (format "%s%sTest.scala"
      (replace-regexp-in-string "app\/" "test\/"
        (file-name-directory buffer-file-name))
      (file-name-nondirectory
        (file-name-sans-extension buffer-file-name)))))

(global-set-key (kbd "s-T") 'jump-to-test)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq scala-indent:use-javadoc-style t)
(define-key global-map (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; whitespace settings
(require 'whitespace)
(setq whitespace-line-column 140)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

;; Turn on yafolding-mode for scala files
(add-hook 'prog-mode-hook
  (lambda () (yafolding-mode)))
(add-hook 'scala-mode-hook
  (lambda () (yafolding-mode)))

;; Pretty print XML
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
