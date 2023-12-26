;;;  -*- lexical-binding: t -*-

(load "fakemake-buildenv" nil t)
(load "fakemake-done" nil t)

(fakemake-check-done compile
  "Source had not been compiled.  Installation can't proceed.")

(defconst root (or (getenv "DESTDIR") "/"))

;; TODO: Fix permissions, both on dirs and individual files
;; Note: permissions on dir site-gentoo.d are fine

(defun fakemake-install ()
  (with-filenames-relative-to root ( sitelisp lispdir etcdir
                                     org-upstream-sources-directory)

    (when (and site-autoloads (file-directory-p "site-gentoo.d"))
      (let ((default-directory (expand-file-name "site-gentoo.d")))
        (do-default-directory-files (file (rx ".el" string-end) nil t)
          (rename-file file (concat fakemake-site-lisp-config-prefix file))))
      (copy-directory "site-gentoo.d"
                      (expand-file-name "site-gentoo.d" sitelisp)
                      t t t)
      (message "Installed site-lisp configuration"))

    (ensure-directory lispdir)
    ;; (unless (cl-every (lambda (file)
    ;;                     (string-match-p (rx ".el" (zero-or-one ?c) string-end)
    ;;                                     file))
    ;;                   fakemake-files-to-install-into-lispdir)
    ;;   (warn "Some files to install into lispdir are not elisp files"))
    (do-files-list (elisp-file fakemake-files-to-install-into-lispdir)
      "Installed %s elisp file%s"
      (when (string-contained-p elisp-file fakemake-autoload-files)
        (message "Installing autoloads file in-source..."))
      (copy-file elisp-file (expand-file-name (file-name-nondirectory
                                               elisp-file)
                                              lispdir)
                 t))

    (when (file-directory-p
           (expand-file-name "contrib" fakemake-project-root))
      (copy-directory (expand-file-name "contrib" fakemake-project-root)
                      lispdir)
      (message "Installed contrib directory"))

    (when (file-directory-p
           (expand-file-name "lisp" fakemake-project-root))
      (ensure-directory etcdir)
      (do-files-list (file (directory-files
                            fakemake-project-root
                            nil
                            (rx string-start
                                (or (not ?.)
                                    (seq ?. (or (not ?.)
                                                (seq ?.
                                                     anything)))))
                            t))
        (let (exception)
          (cond
           ((setq exception (cl-member file fakemake-special-filenames
                                       :test #'string-matched-p))
            (do-not-count "as specially recognized"))
           ((setq exception (cl-member file fakemake-install-regexps-blacklist
                                       :test #'string-matched-p))
            (do-not-count "as blacklisted by %s" (car exception)))
           ((file-directory-p file)
            (copy-directory file etcdir nil t))
           ((string-match-p (rx ".org" string-end) file)
            (do-not-count "as controlled by the variable %s"
                          'org-upstream-sources-directory))
           (t
            (copy-file file etcdir t)))))
      (message "Installed project's root files into etcdir"))

    (when (file-directory-p "etc")
      (ensure-directory etcdir)
      (copy-directory (expand-file-name "etc" fakemake-project-root)
                      etcdir
                      t nil t)
      (message "Installed files from etc"))

    (when org-upstream-sources-directory
      ;; Install org files found in project's root
      (ensure-directory org-upstream-sources-directory)
      (do-default-directory-files (org-file
                                   (rx ".org" string-end) nil t)
        "Installed %s Org file%s"
        (if (or (and (not (memq 'test use-flags))
                     (string-contained-p
                      org-file
                      (ignore-errors fakemake-org-files-for-testing-in-order)
                      :key #'file-name-base))
                (file-symlink-p org-file))
            (do-not-count)
          (copy-file org-file (file-name-as-directory
                               org-upstream-sources-directory)
                     t))))

    (when (and (memq 'test use-flags)
               (file-directory-p "tests"))
      (copy-directory "tests" etcdir)
      (message "Installed tests")))
  nil)

(fakemake-install)
