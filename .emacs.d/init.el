(require 'package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; ensure every single package installed via Package.el is loaded in ready to be customized
(package-initialize)

(menu-bar-mode -1)

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(require 'dependencies)

(defconst custom-file-path (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file-path) (write-region "" nil custom-file-path))

(setq custom-file custom-file-path)
(load custom-file-path)

;; my once-and-for-all favourite theme
(require 'monokai-theme)
(setq custom-safe-themes t)
(load-theme 'monokai)

;; (set-default-font "Source Code Pro")
(set-face-attribute 'default nil :height 90)

;; handy plugin for moving entire lines up/down (like C-S-ArrowUp/ArrowDown in Sublime Text)
(require 'move-text)
(move-text-default-bindings)

(require 'multiple-cursors)
(global-set-key (kbd "C-c l") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
(when mc/keymap
  (define-key mc/keymap (kbd "<return>") 'newline))

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c f") 'projectile-find-file)
;; Projectile no longer dispatches to ido on its own (it reads through plain
;; `completing-read', which `ido-everywhere' does not hook), so route its
;; completion through ido explicitly.
(setq projectile-completion-system
      (lambda (prompt choices)
        (ido-completing-read prompt choices)))

;; Sublime-style fuzzy scoring for ido: "tape/meta.fbs" finds
;; library/tape/protocol/meta.fbs, while scatter matches over long paths
;; (e.g. "abc.cpp" onto "av/library/.../bc.cpp") sink to the bottom.
(require 'flx-ido)
(flx-ido-mode 1)
;; rank and highlight at any candidate count (the av monorepo has ~41k
;; files); scoring that many takes ~0.5s, tolerable because the
;; abort-on-input advice below keeps it off the typing path
(setq flx-ido-threshold 50000)

;; never let candidate matching block a keystroke: a key arriving while the
;; match recomputes aborts it (the previous list stays visible for that
;; instant) and that same key's own redisplay recomputes from scratch, so
;; only the final pause pays the full matching cost.  Known small race: RET
;; hit mid-recompute selects from the previous list.
(define-advice ido-set-matches (:around (orig) abort-on-input)
  (while-no-input (funcall orig)))
;; ido marks the candidate RET selects by replacing its face property
;; outright, which wipes flx's per-character match highlights on it; keep
;; ido's faces off and paint the selection bar in the `ido-completions'
;; advice below instead
(setq ido-use-faces nil)
;; region-gray bar behind the selected candidate, the same look as a buffer
;; selection, with its plain text tinted green; the yellow match-character
;; highlights stay on top (the advice below layers this face beneath them)
(set-face-attribute 'ido-first-match nil
                    :foreground monokai-green
                    :background monokai-highlight
                    :weight 'unspecified)
;; monokai themes flx's highlight as plain blue, which doesn't stand out;
;; use the theme's yellow (as ido-first-match had) and bold instead
(with-eval-after-load 'flx
  (set-face-attribute 'flx-highlight-face nil
                      :foreground monokai-yellow
                      :weight 'bold
                      :underline nil))

;; show the typed pattern in the same yellow as its matches in the
;; candidates, so it stands out from the white candidate list
(defun ido-highlight-input ()
  (when (bound-and-true-p ido-cur-item)
    (face-remap-add-relative 'default :foreground monokai-yellow :weight 'bold)))
(add-hook 'minibuffer-setup-hook 'ido-highlight-input)

;; the face remap above would also repaint the candidate list (ido inserts
;; it into the minibuffer as plain text when `ido-use-faces' is nil), so pin
;; the list to the theme's regular foreground; flx's per-character
;; highlights carry their own face and stay on top
;; the advice is named so re-evaluating init.el replaces it instead of
;; stacking another copy alongside the old one
(define-advice ido-completions (:filter-return (completions) monokai-candidate-faces)
  ;; the selection bar goes on first so it sits below the flx highlights
  ;; but above the foreground pin added after it: matched characters stay
  ;; yellow, the rest of the selected candidate turns green
  (when ido-matches
    (let* ((first-match (ido-name (car ido-matches)))
           (start (string-search first-match completions)))
      (when start
        (add-face-text-property start (+ start (length first-match))
                                'ido-first-match t completions))))
  (add-face-text-property 0 (length completions)
                          (list :foreground monokai-foreground :weight 'normal)
                          t completions)
  completions)

(require 'expand-region)
;; alternate between expand-region and mc/mark-next-like-this
(defun expand-or-mark-next-like-this () (interactive) (if (use-region-p)
	                                                        (mc/mark-next-like-this 1)
	                                                      (er/expand-region 1)))
(global-set-key (kbd "C->") 'expand-or-mark-next-like-this)

;; incremental search auto wrap
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

(require 'auto-complete)
(global-auto-complete-mode)
(add-to-list 'ac-modes 'prog-mode)

(require 'ido)
(define-key ido-common-completion-map (kbd "C-a") nil)
(ido-mode)
(ido-everywhere)
(setq ido-enable-flex-matching t)

(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 100 2))
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(show-paren-mode)
(setq make-backup-files nil)
(setq use-dialog-box nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; stop asking full yes or no and be fine with y/n instead
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c r") 'revert-buffer)

(require 'flatbuffers-mode)

(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.h\\.jinja2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\.jinja2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\.jinja2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\.jinja2\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.py\\.jinja2\\'" . python-mode))

(require 'perfect-margin)
(perfect-margin-mode)

(defun remove-prefix (prefix string)
  (if (string= (substring string 0 (length prefix)) prefix)
      (substring string (length prefix))
    string))

(defun remove-suffix (suffix string)
  (let* ((suffix_len (length suffix))
         (suffix_pos (- (length string) suffix_len)))
    (if (string= (substring string suffix_pos) suffix)
        (substring string 0 suffix_pos)
      string)))

(defun print-github-link ()
  "Print a link to the GitHub tree of the current buffer"
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (if
        (eq (vc-backend filename) 'Git)
        (let* ((git_root (expand-file-name (vc-git-root default-directory)))
               (path (remove-prefix git_root filename))
               (branch (string-trim (vc-git--run-command-string nil "branch" "--show-current")))
               (commit (string-trim (vc-git--run-command-string nil "rev-parse" "HEAD")))
               (branch_or_commit (if (string-empty-p branch) commit branch))
               (remote (string-trim (vc-git--run-command-string nil "remote")))
               (remote_url (string-trim (vc-git--run-command-string nil "remote" "get-url" remote)))
               (organization_and_repo (if (string-prefix-p "https://" remote_url)
                                          (remove-suffix "/" (remove-prefix "https://github.com/" remote_url))
                                        (remove-suffix ".git" (remove-prefix "git@github.com:" remote_url))))
               (line_start (number-to-string (line-number-at-pos (if (use-region-p) (region-beginning) nil))))
               (line_end (number-to-string (line-number-at-pos (region-end)))))
          (princ
           (concat "https://github.com/"
                   organization_and_repo
                   "/blob/"
                   branch_or_commit
                   "/"
                   path
                   "#L"
                   line_start
                   (if (use-region-p)
                       (concat
                        "-L"
                        line_end)
                     "")) t))
      (princ "File is not in Git")
      )))
(global-set-key (kbd "C-c g") 'print-github-link)


;; allow stepping over CamelCase word parts with M-f/M-b
(global-subword-mode 1)
