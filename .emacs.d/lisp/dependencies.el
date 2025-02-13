(require 'cl)

(defconst required-packages
  '(monokai-theme move-text fiplr multiple-cursors
		  expand-region auto-complete projectile flatbuffers-mode perfect-margin nord-theme solarized-theme nordic-night-theme))

(defun required-packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (required-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'dependencies)
