(x-focus-frame nil)

;; Use a proper font!
(set-frame-font "Hack-18" nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "8453c6ba2504874309bdfcda0a69236814cefb860a528eb978b5489422cb1791" "d63e365e57d777c32290dbc672b410f32a112eb5d4b398015cfe822f9a8c07fd" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 ;; Start in fullscreen mode
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Configure for Norwegian MacBook Pro keybard layout
(setq default-input-method "MacOSX")
(setq mac-option-modifier nil
      mac-left-command-modifier 'super
      mac-right-command-modifier 'meta
      mac-allow-anti-aliasing t
      x-select-enable-clipboard t)

;; ****************************************************
;; Package management
;; ****************************************************
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; List the packages I want
(setq package-list
  '(color-theme
    color-theme-solarized
    ensime
    expand-region
    find-file-in-repository
    key-chord
    magit
    markdown-mode
    neotree
    powerline
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

;;(theme-looper-set-theme-set (list 'deeper-blue 'tango-dark 'tsdh-dark 'wheatgrass))

;; ****************************************************
;; Scala related
;; ****************************************************

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq ensime-sem-high-faces '(
  (var . (:foreground "#FCE3E5"))
  (val . (:foreground "#DDDDDD"))
  (varField . (:foreground "#FCE3E5"))
  (valField . (:foreground "#DDDDDD"))
  (functionCall . (:bold :foreground "#A166FF"))
  (param . (:foreground "#FFFFFF"))
  (class . font-lock-type-face)
  (trait . (:foreground "#C9B8FF"))
  (object . (:foreground "#026DF7"))
  (package . font-lock-preprocessor-face)
  (deprecated . (:strike-through "dark gray"))
  ;;(implicitConversion . (:underline (:style wave :color "blue"))
  ;;(implicitParams . (:underline (:style wave :color "blue"))
))

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

(setq scala-indent:use-javadoc-style t)
(define-key global-map (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

;; Turn on yafolding-mode for scala files
(add-hook 'prog-mode-hook
  (lambda () (yafolding-mode)))
(add-hook 'scala-mode-hook
  (lambda () (yafolding-mode)))


;; ****************************************************
;; General editor stuff
;; ****************************************************

;; Powerline mode lines
;;(require 'powerline)
;;(powerline-default-theme)

;; Color theme stuff...
(require 'color-theme)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'Hepatica t)

(global-linum-mode 1)
;; Always pick up the most recent file from the filesystem
(global-auto-revert-mode 1)

;; Some general-purpose key bindings
(global-set-key (kbd "S-z") 'undo)
(global-set-key (kbd "C-l") 'goto-line) ; [Ctrl]-l]

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(define-key global-map (kbd "RET") 'newline-and-indent)

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
(global-set-key (kbd "s-d") 'neotree-toggle)
(neotree)

;; whitespace settings
(require 'whitespace)
(setq whitespace-line-column 140)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

(require 'find-file-in-repository)

(require 'yasnippet)
(yas-global-mode 1)

(require 'window-numbering)
(window-numbering-mode 1)

;; Exit emacs w/o prompts
(require 'cl)
(defadvice save-buffers-kill-emacs
  (around no-query-kill-emacs activate)
  (flet ((process-list ())) ad-do-it))

(defun save-all () (interactive) (save-some-buffers t))
(global-set-key (kbd "S-s") 'save-all)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

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
