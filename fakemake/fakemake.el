;;;  -*- lexical-binding: t -*-

;; Version: 20240603.134558

(setq debugger-stack-frame-as-list t)

(load "fakemake-buildenv" nil t)
(load "fakemake-done" nil t)

;; Don't litter.
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(expand-file-name
                                        ".saves" user-emacs-directory)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control nil)

(defun fakemake-message (format-string &rest args)
  (apply #'message (concat "%s: " format-string)
         'fakemake args))

(defvar fakemake-ignore-null-license nil)

(defvar fakemake-org-files-for-testing-in-order)
;; (defvar fakemake-source-files-for-testing-in-order)

(fakemake-done-defvar fakemake-elisp-source-files-in-order nil)

(fakemake-done-defvar fakemake-files-to-install-into-lispdir nil)
(fakemake-done-defvar fakemake-autoload-files nil)

(fakemake-done-defvar fakemake-special-filenames
  (list (rx string-start "Makefile")
        (rx string-start "README")
        (rx string-start "site-gentoo.d" string-end)
        (rx string-start ".git" string-end)
        (rx string-start ".gitignore" string-end)
        (rx string-start "lisp" string-end)
        (rx string-start "contrib" string-end)
        (rx string-start "fakemake" string-end)
        (rx string-start "etc" string-end)))

(defvar fakemake-files-to-load-for-testing-in-order nil)

(defun fakemake--file-from-elisp-source-directory-p (file)
  (string-contained-p (file-name-directory (expand-file-name
                                            file fakemake-project-root))
                      fakemake-elisp-source-directories
                      :key #'expand-file-name))

(defconst org-sources-directory (or org-local-sources-directory
                                    org-upstream-sources-directory)
  "Directory with Org source files to link Elisp source files to.

Meant to be set by fakemake.")

(when org-sources-directory
  (unless (stringp org-sources-directory)
    (error "Error: %s is not a string: %s"
           'org-sources-directory org-sources-directory))
  (unless (string-prefix-p "/" org-sources-directory)
    (error "Error: Org sources directory is ambiguous: %s"
           org-sources-directory))
  (fakemake-message "%s is set to %s"
                    'org-sources-directory org-sources-directory)
  ;; TODO: This message should not be seen after prepare;
  ;; probably the whole form should not be evaluated then.
  (fakemake-message "Links in el source will point to it"))

(defvar fakemake--redirect-links org-sources-directory)

;; this is likely only needed when org had been ripped off Emacs
;; and installed separately
(require 'org)

(require 'org-element)
(setq org-element-cache-persistent nil)

;; silence some messages
(defalias 'org-development-elisp-mode 'org-mode)

(defun fakemake-clean ()
  (do-files-recursively (file fakemake-project-root
                              :do-not-descend '(".git/"))
    (unless (string-contained-p file fakemake-original-files)
      (if (file-directory-p file)
          (delete-directory file t)
        (delete-file file)))))

(defun fakemake-process-special-dirs ()
  (when (file-directory-p "contrib")
    (cl-pushnew "contrib" fakemake-elisp-source-directories
                :test #'string-equal))
  (when (file-directory-p "lisp")
    (setq fakemake-elisp-source-directories
          (cl-delete fakemake-project-root
                     fakemake-elisp-source-directories
                     :test #'string-equal))
    (cl-pushnew "lisp" fakemake-elisp-source-directories
                :test #'string-equal)))

(unless (fakemake-donep 'compile)
  (when (memq 'test use-flags)
    (let (testing-thunk-is-found)
      (cond
       ((boundp 'fakemake-org-files-for-testing-in-order)
        (if (listp fakemake-org-files-for-testing-in-order)
            (setq fakemake-org-files-in-order (append
                                               fakemake-org-files-in-order
                                               fakemake-org-files-for-testing-in-order)
                  testing-thunk-is-found t)
          (error "%s must be a list but its value is %s"
                 'fakemake-org-files-for-testing-in-order
                 fakemake-org-files-for-testing-in-order)))
       (t
        (fakemake-message "Guessing how testing %s is arranged in it..."
                          fakemake-feature)
        (if-let ((tests-org-source (file-exists-p+
                                    (format "%s-tests.org" fakemake-feature))))
            (progn
              (setq fakemake-org-files-for-testing-in-order
                    (list tests-org-source))
              (if (string-contained-p tests-org-source
                                      fakemake-org-files-in-order)
                  (fakemake-message "Found org source file %s already mentioned in the system definition"
                                    tests-org-source)
                (fakemake-message "Registering org source file %s" tests-org-source)
                (setq fakemake-org-files-in-order
                      (append fakemake-org-files-in-order
                              fakemake-org-files-for-testing-in-order)
                      testing-thunk-is-found t)))
          (setq fakemake-org-files-for-testing-in-order nil)
          (fakemake-message "No bundled testing found"))))
      (when (and testing-thunk-is-found
                 fakemake-org-files-for-testing-in-order)
        ;; What follows is a hack.
        ;; TODO: set up output-files functions or variables instead,
        ;; record them in directory or file .fakemake-state
        ;; or some place like this
        (setq fakemake-files-to-load-for-testing-in-order
              (mapcar (lambda (x) (concat (file-name-base x) ".el"))
                      fakemake-org-files-for-testing-in-order))
        (fakemake-message "Blindly assuming testing code will be in %s"
                          (mapconcat #'identity
                                     fakemake-files-to-load-for-testing-in-order
                                     ", "))))
    (fakemake-done-defvar fakemake-org-files-for-testing-in-order)
    (fakemake-done-defvar fakemake-files-to-load-for-testing-in-order)
    ;; (fakemake-done-defvar fakemake-source-files-for-testing-in-order)
    ))

(defun insert-gpl-3-license (initial-year-as-string authors &optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer (find-file-noselect
                        (expand-file-name "COPYING" fakemake-project-root))
    (goto-char (point-min))
    (save-match-data
      (re-search-forward
       (rx line-start "How to Apply These Terms to Your New Programs"))
      (re-search-forward
       (rx line-start
           "<one line to give the program's name and a brief idea of what it does.>"))
      (let ((start (with-current-buffer buffer (point))))
        (re-search-forward (rx line-start))
        (let ((copyright-pattern
               (if (not (looking-at-p "Copyright"))
                   (error "Can't find copyright line in the template")
                 (buffer-substring-no-properties
                  (point) (progn (end-of-line) (point)))))
              (years (let ((current-year (format-time-string "%Y")))
                       (if (string-equal initial-year-as-string current-year)
                           current-year
                         (concat initial-year-as-string "--" current-year)))))
          (with-current-buffer buffer
            (insert
             (fold (lambda (string rule)
                     (replace-regexp-in-string (car rule) (cdr rule) string))
                   copyright-pattern
                   `((,(rx "<year>") . ,years)
                     (,(rx "<name of author>") . ,authors))))))
        (let ((rest-of-the-license (buffer-substring-no-properties
                                    (point)
                                    (progn (re-search-forward
                                            (rx line-start "Also add"))
                                           (beginning-of-line) (point)))))
          (with-current-buffer buffer
            (insert rest-of-the-license)
            (comment-region start (point))))))
    (kill-buffer)))

(defun license-normalize (license)
  (when license
    (alist-get
     license
     ;; TODO: Generate variations
     '(
       ;; gpls
       (( "gpl-3"    "gpl-3+"    "GPL-3"    "GPL-3+")     .  "gpl-3.0")
       (( "gpl-2"    "gpl-2+"    "GPL-2"    "GPL-2+")     .  "gpl-2.0")
       (("agpl-3"   "agpl-3+"   "AGPL-3"   "AGPL-3+")     . "agpl-3.0")
       (("lgpl-3"   "lgpl-3+"   "LGPL-3"   "LGPL-3+")     . "lgpl-3.0")
       (("lgpl-2.1" "lgpl-2.1+" "LGPL-2.1" "LGPL-2.1+")   . "lgpl-2.1")
       (("gfdl-1.3" "gfdl-1.3+" "GFDL-1.3" "GFDL-1.3+")   . "gfdl-1.3")
       ;; and alike (still allows +)
       (("lppl-1.3" "lppl-1.3+" "LPPL-1.3" "LPPL-1.3+")   . "lppl-1.3c")
       ;; other versioned
       (("cc-by-sa-4" "CC-BY-SA-4")                       . "cc-by-sa-4.0")
       ;; sometimes-not-versioned
       ((     "epl-1" "epl"           "EPL-1")            . "epl-1.0")
       ((  "apache-2" "apache"     "Apache-2" "Apache")   . "apache-2.0")
       (("artistic-2" "artistic" "Artistic-2" "Artistic") . "artistic-2.0")
       ;; quasi-versioned
       (("bsd-2" "BSD-2")                                 . "bsd-2-clause")
       (("bsd-3" "BSD-3")                                 . "bsd-3-clause")
       ;; rest
       (("unlicense" "UNLICENSE" "Unlicense")             . "unlicense")
       (("mozilla"   "MOZILLA"   "Mozilla")               . "mozilla")
       (("mit"       "MIT")                               . "mit"))
     license nil
     (lambda (key l) (string-contained-p l key)))))

(cl-defun fakemake--license-file-exists-p (&optional (directory
                                                      fakemake-project-root))
  (cl-some (lambda (x) (file-exists-p+ (expand-file-name x directory)))
           '("COPYING" "LICENSE")))

(declare-function package-desc-extras "package")
(declare-function package-desc-reqs "package")
(declare-function package-desc-summary "package")

(when (boundp 'fakemake-package-requires)
  (fakemake-done-defvar fakemake-package-version
    (format-time-string "0.0.%Y%m%d.%H%M%S")))

(defun elisp-insert-file-header (name description &optional local-variables)
  (insert ";;; "
          ;; We should probably use comment- functions
          ;; but I couldn't figure out this interface.
          )
  (insert name)
  (when description (insert " --- " description))
  (when local-variables
    (insert ?\s " -*-" ?\s)
    (cl-loop
     for (key value . tail) on local-variables by #'cddr
     do (insert (ensure-string key) ": "
                (ensure-string value))
     (when tail (insert "; ")))
    (insert " -*-"))
  (insert ?\n))

(cl-defun elisp-insert-header ( &key
                                name description local-variables
                                first-publication-year authors package-desc)
  (elisp-insert-file-header name description local-variables)
  (let ((start (point)))
    (when (or authors first-publication-year)
      (insert ?\n)
      (insert "Copyright (C)")
      (when first-publication-year
        (insert ?\s
                (let ((current-year (format-time-string "%Y")))
                  (fif (not (string-equal
                             current-year
                             first-publication-year))
                    (lambda (first-year)
                      (concat first-year "--" current-year))
                    first-publication-year))))
      (when authors (insert ?\s authors)))
    (when package-desc
      (insert ?\n)
      (cl-loop for (k . v) in (package-desc-extras package-desc)
               when v
               do (cl-case k
                    (:authors
                     (let ((multiple-authors-indent 0))
                       (insert (format "Author%s: " (if (cdr v) "s" "")))
                       (when (cdr v) (setq multiple-authors-indent
                                           (current-column)))
                       (if v (cl-flet ((insert-author (author)
                                         (insert (format "%s <%s>\n"
                                                         (car author)
                                                         (cdr author)))))
                               (insert-author (pop v))
                               (cl-loop for a in v
                                        do
                                        (cl-loop repeat multiple-authors-indent
                                                 do (insert ?\s))
                                        (insert-author a)))
                         "Unknown\n")))
                    (:maintainer
                     (insert "Maintainer: " (if v (format "%s <%s>\n"
                                                          (car v) (cdr v))
                                              "Unknown\n")))
                    (:url
                     (insert "URL: " v ?\n))))
      (insert "Version: " fakemake-package-version ?\n)
      (insert "Package-Requries: " (format "%S" (package-desc-reqs package-desc))
              ?\n)
      (cl-loop for (k . v) in (package-desc-extras package-desc)
               do (cl-case k
                    ((:authors :maintainer :url))
                    (otherwise
                     (insert (ensure-string k)
                             (format ": %S\n" v))))))
    (when (> (point) start) (comment-region start (point)))))

(defun insert-elisp-preamble ( tangled-file
                               &optional local-variables package-desc)
  (with-current-buffer (find-file-noselect tangled-file)
    (let ((org-file (file-exists-p+ (concat (file-name-sans-extension
                                             tangled-file)
                                            ".org"))))
      (let ((name (file-name-nondirectory tangled-file))
            (description (cond
                          ;; Normally, such a function
                          ;; would check its argument PACKAGE-DESC first
                          ;; We check org-file first only because
                          ;; fakemake is very specifically made for Org.
                          (org-file
                           (with-current-buffer (find-file-noselect org-file)
                             (or (save-excursion
                                   (goto-char (point-min))
                                   (save-match-data
                                     (when (re-search-forward
                                            (rx line-start "#+description: ")
                                            nil t)
                                       (buffer-substring-no-properties
                                        (point) (line-end-position)))))
                                 (save-excursion
                                   (goto-char (point-min))
                                   (save-match-data
                                     (when (re-search-forward
                                            (rx line-start "#+subtitle: ")
                                            nil t)
                                       (buffer-substring-no-properties
                                        (point) (line-end-position))))))))
                          ((and package-desc
                                (package-desc-summary package-desc))))))
        ;; (when package-desc
        ;;   ;; this setf can't be found
        ;;   (setf (package-desc-summary package-desc) description))
        (setq local-variables (plist-put local-variables :lexical-binding t))
        (let ((license (or (license-normalize
                            (or (when org-file
                                  (with-current-buffer (find-file-noselect org-file)
                                    (save-match-data
                                      (when (re-search-forward
                                             (rx line-start "#+license: ")
                                             nil t)
                                        (buffer-substring-no-properties
                                         (point) (line-end-position))))))
                                (ignore-errors (stringp+ fakemake-license))))
                           (ignore-errors fakemake-license))))
          (cond
           ((null license)
            (if (not fakemake-ignore-null-license)
                (error "No license specified for %s" name)
              (goto-char (point-min))
              (apply #'elisp-insert-header
                     :first-publication-year
                     fakemake-first-publication-year-as-string
                     :authors fakemake-authors
                     (plist-with-keys name description local-variables
                                      package-desc))
              (save-buffer)))
           ((aand (string-equal "gpl-3.0" license)
                  (fakemake--license-file-exists-p)
                  (with-current-buffer (find-file-noselect it)
                    (and (ignore-errors
                           (save-excursion
                             (goto-char (point-min))
                             (search-forward
                              "How to Apply These Terms to Your New Programs"
                              nil t))
                           t)
                         (ignore-errors
                           (save-excursion
                             (goto-char (point-min))
                             (save-match-data
                               (and (re-search-forward
                                     (rx line-start
                                         "GNU GENERAL PUBLIC LICENSE")
                                     nil t)
                                    (looking-at-p
                                     (rx (one-or-more whitespace)
                                         "Version 3" (not digit))))))))))
            ;; The obsessive case, left here mostly due to personal tastes
            (goto-char (point-min))
            (apply #'elisp-insert-header
                   (plist-with-keys name description local-variables
                                    package-desc))
            (insert ?\n)
            (insert-gpl-3-license
             fakemake-first-publication-year-as-string
             fakemake-authors)
            (save-buffer))
           ((eq 'custom license)
            (let ((license-file (fakemake--license-file-exists-p)))
              (cl-assert license-file)
              (apply #'elisp-insert-header
                     :first-publication-year
                     fakemake-first-publication-year-as-string
                     :authors fakemake-authors
                     (plist-with-keys name description local-variables
                                      package-desc))
              (let ((start (point)))
                (insert ?\n ?\n)
                (let ((license-buffer (find-file-noselect license-file)))
                  (insert-buffer-substring license-buffer)
                  (kill-buffer license-buffer))
                (comment-region start (point))))
            (insert ?\n)
            (save-buffer))
           ((fboundp 'lice)
            (apply #'elisp-insert-header
                   :first-publication-year
                   fakemake-first-publication-year-as-string
                   :authors fakemake-authors
                   (plist-with-keys name description local-variables
                                    package-desc))
            (insert ?\n ?\n)
            (let ((lice:header-spec '(lice:insert-license))
                  (lice:program-name fakemake-feature-full-name))
              (lice license))
            (save-buffer))
           (t
            (let (license-file)
              (if (not (setq license-file (fakemake--license-file-exists-p)))
                  (error "Don't know how to deal with the license")
                (apply #'elisp-insert-header
                       :first-publication-year
                       fakemake-first-publication-year-as-string
                       :authors fakemake-authors
                       (plist-with-keys name description local-variables
                                        package-desc))
                (insert ?\n ?\n)
                (let* ((start (point))
                       (end (+ start
                               (cadr
                                (insert-file-contents-literally
                                 license-file)))))
                  (comment-region start end))
                (save-buffer))))))))))

(defun insert-newline-to-please-unix () (insert ?\n))

(defun insert-elisp-postamble (file)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (unless (string-contained-p (file-name-base file)
                                ;; Sometimes bad bytecode is produced,
                                ;; and omitting `provide' kinda helps.
                                (ignore-errors fakemake-exceptions-to-provide))
      (insert ?\n)
      (prin1 `(provide ',(intern (file-name-base file))) (current-buffer)))
    (insert ?\n ?\n
            (file-name-nondirectory file) " ends here")
    (comment-region (line-beginning-position) (line-end-position) 3)
    (insert-newline-to-please-unix)
    (save-buffer) (kill-buffer))
  file)

(require 'ob-core)
(require 'ob-tangle)

(defvar ob-flags)

(require 'ol)

(defun fakemake--org-links-disabled-p (alist)
  (aand (assq :comments alist)
        (or (stringp+ (cdr it))
            (aif (functionp+ (cdr it))
                (or (stringp+ (funcall it))
                    (error "Element of unexpected type returned by the closure in Org parameter :comments"))
              (error "Element of unexpected type in Org parameter :comments")))
        (cl-find it '("no" "org") :test #'string-equal)))

(defmacro with-org-links-redirection-to (directory &rest body)
  (declare (indent 1))
  `(let ((fakemake--redirect-links ,directory))
     (let ((org-babel-default-header-args
            org-babel-default-header-args))
       (unless (fakemake--org-links-disabled-p
                org-babel-default-header-args)
         (push '(:comments . "link")
               ;; It is unclear
               ;; if we can actually push
               ;; a constant cons here.
               org-babel-default-header-args))
       ,@body)))

(defun fakemake--tangle-collect-blocks/mv (&optional lang-re tangle-file)
  "Collect source blocks in the current Org file.
Return an association list of language and source-code block
specifications of the form used by `org-babel-spec-to-string'
grouped by tangled file name.

Optional argument LANG-RE can be used to limit the collected
source code blocks by languages matching a regular expression.

Optional argument TANGLE-FILE can be used to limit the collected
code blocks by target file."
  (let ((counter 0) (block-counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (cl-incf block-counter)
      (let ((current-heading-pos
	     (org-with-wide-buffer
	      (org-with-limited-levels (outline-previous-heading)))))
	(if (eq last-heading-pos current-heading-pos) (cl-incf counter)
	  (setq counter 1)
	  (setq last-heading-pos current-heading-pos)))
      (unless (or (org-in-commented-heading-p)
		  (org-in-archived-heading-p))
	(let* ((info (org-babel-get-src-block-info 'light))
	       (src-lang (nth 0 info))
	       (src-tfile (cdr (assq :tangle (nth 2 info)))))
	  (unless (or (string= src-tfile "no")
		      (and tangle-file (not (equal tangle-file src-tfile)))
		      (and lang-re (not (string-match-p lang-re src-lang))))
	    ;; Add the spec for this block to blocks under its tangled
	    ;; file name.
	    (let ((block (fakemake--tangle-single-block counter)))
	      (dolist (src-tfile (ensure-list
                                  (cdr (assq :tangle (nth 4 block)))))
                (let* ((file-name (org-babel-effective-tangled-filename
                                   (nth 1 block) src-lang src-tfile))
                       (by-fn (assoc file-name blocks)))
                  (if by-fn (setcdr by-fn (cons (cons src-lang block) (cdr by-fn)))
		    (push (cons file-name (list (cons src-lang block))) blocks)))))))))
    ;; Ensure blocks are in the correct order.
    (cl-values (dolist (b (nreversef blocks) blocks)
                 (nreversef (cdr b)))
               block-counter)))

(defun fakemake--tangle-collect-blocks (&optional lang-re tangle-file)
  (cl-multiple-value-bind (blocks counter) (fakemake--tangle-collect-blocks/mv
                                            lang-re tangle-file)
    blocks))

(defun fakemake--tangle-single-block (block-counter &optional only-this-block)
  "Collect the tangled source for current block.
Return the list of block attributes needed by
`org-babel-tangle-collect-blocks'.  When ONLY-THIS-BLOCK is
non-nil, return the full association list to be used by
`org-babel-tangle' directly."
  (let* ((info (org-babel-get-src-block-info))
	 (start-line
	  (save-restriction (widen)
			    (+ 1 (line-number-at-pos (point)))))
	 (file (buffer-file-name (buffer-base-buffer)))
	 (src-lang (nth 0 info))
	 (params (nth 2 info))
	 (extra (nth 3 info))
         (coderef (nth 6 info))
	 (cref-regexp (org-src-coderef-regexp coderef))
	 (link (let* (
                      ;; The created link is transient.  Using ID is
                      ;; not necessary, but could have side-effects if
                      ;; used.  An ID property may be added to
                      ;; existing entries thus creatin unexpected file
                      ;; modifications.
                      (org-id-link-to-org-use-id nil)
                      (l (org-no-properties (org-store-link nil))))
                 (and (string-match org-link-bracket-re l)
                      (match-string 1 l))))
	 (source-name
	  (or (nth 4 info)
	      (format "%s:%d"
		      (or (ignore-errors (nth 4 (org-heading-components)))
			  "No heading")
		      block-counter)))
	 (expand-cmd (intern (concat "org-babel-expand-body:" src-lang)))
	 (assignments-cmd
	  (intern (concat "org-babel-variable-assignments:" src-lang)))
	 (body
	  ;; Run the tangle-body-hook.
          (let ((body (if (org-babel-noweb-p params :tangle)
			  (org-babel-expand-noweb-references info)
			(nth 1 info))))
	    (with-temp-buffer
	      (insert
	       ;; Expand body in language specific manner.
	       (cond ((assq :no-expand params) body)
		     ((fboundp expand-cmd) (funcall expand-cmd body params))
		     (t
		      (org-babel-expand-body:generic
		       body params (and (fboundp assignments-cmd)
					(funcall assignments-cmd params))))))
	      (when (string-match "-r" extra)
		(goto-char (point-min))
		(while (re-search-forward cref-regexp nil t)
		  (replace-match "")))
	      (run-hooks 'org-babel-tangle-body-hook)
	      (buffer-string))))
	 (comment
	  (when (or (string= "both" (cdr (assq :comments params)))
		    (string= "org" (cdr (assq :comments params))))
	    ;; From the previous heading or code-block end
	    (funcall
	     org-babel-process-comment-text
	     (buffer-substring
	      (max (condition-case nil
		       (save-excursion
			 (org-back-to-heading t) ; Sets match data
			 (match-end 0))
		     (error (point-min)))
		   (save-excursion
		     (if (re-search-backward
			  org-babel-src-block-regexp nil t)
			 (match-end 0)
		       (point-min))))
	      (point))))))
    (cl-flet ((link (src-tfile)
                (cond
                 ((and fakemake--redirect-links
                       (string-match org-link-types-re link)
		       (string= (match-string 1 link) "file"))
                  (concat "file:"
                          (expand-file-name
                           (file-relative-name (substring link (match-end 0))
                                               fakemake-project-root)
                           fakemake--redirect-links)))
                 ((and org-babel-tangle-use-relative-file-links
		       (string-match org-link-types-re link)
		       (string= (match-string 1 link) "file"))
	          (concat "file:"
		          (file-relative-name (substring link (match-end 0))
					      (file-name-directory
                                               src-tfile))))
	         (t link))))
      (let ((buffer-fn (if org-babel-tangle-use-relative-file-links
	                   (file-relative-name file)
	                 file))
            (maybe-trimmed-body (if org-src-preserve-indentation
	                            (org-trim body t)
	                          (org-trim (org-remove-indentation body))))
            (tangle-to (cdr (assq :tangle params))))
        (cl-flet ((org-tangle-datum (link params)
                    (list start-line
	                  buffer-fn
	                  link
	                  source-name
	                  params
	                  maybe-trimmed-body
	                  comment)))
          (if only-this-block
              (cl-loop for src-tfile in (ensure-list tangle-to)
                       with params-sans-tangle =
                       (cl-remove :tangle params :key #'car :test #'eq)
                       collect
                       (cons (org-babel-effective-tangled-filename
                              buffer-fn src-lang src-tfile)
                             (list
                              (cons src-lang
                                    (org-tangle-datum (link src-tfile)
                                                      (cons
                                                       (cons :tangle src-tfile)
                                                       params-sans-tangle))))))
            (org-tangle-datum
             (if (and (or org-babel-tangle-use-relative-file-links
                          fakemake--redirect-links)
                      (listp tangle-to))
                 (mapcar #'link tangle-to)
               (link tangle-to))
             params)))))))

(defun fakemake--tangle (&optional arg target-file lang-re)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.
With one universal prefix argument, only tangle the block at point.
When two universal prefix arguments, only tangle blocks for the
tangle file of the block at point.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG-RE can
be used to limit the exported source code blocks by languages
matching a regular expression."
  (interactive "P")
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; Possibly Restrict the buffer to the current code block
  (save-restriction
    (save-excursion
      (when (equal arg '(4))
	(let ((head (org-babel-where-is-src-block-head)))
	  (if head
	      (goto-char head)
	    (user-error "Point is not in a source code block"))))
      (let ((block-counter 0)
	    (org-babel-default-header-args
	     (if target-file
		 (org-babel-merge-params org-babel-default-header-args
					 (list (cons :tangle target-file)))
	       org-babel-default-header-args))
	    (tangle-file
	     (when (equal arg '(16))
	       (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
		   (user-error "Point is not in a source code block"))))
	    path-collector)
	(mapc ;; map over file-names
	 (lambda (by-fn)
	   (let ((file-name (car by-fn)))
	     (when file-name
               (let ((lspecs (cdr by-fn))
		     (fnd (file-name-directory file-name))
		     modes make-dir she-banged lang)
	         ;; drop source-blocks to file
	         ;; We avoid append-to-file as it does not work with tramp.
	         (with-temp-buffer
		   (mapc
		    (lambda (lspec)
		      (let* ((block-lang (car lspec))
			     (spec (cdr lspec))
			     (get-spec (lambda (name) (cdr (assq name (nth 4 spec)))))
			     (she-bang (let ((sheb (funcall get-spec :shebang)))
				         (when (> (length sheb) 0) sheb)))
			     (tangle-mode (funcall get-spec :tangle-mode)))
		        (unless (string-equal block-lang lang)
			  (setq lang block-lang)
			  (let ((lang-f (org-src-get-lang-mode lang)))
			    (when (fboundp lang-f) (ignore-errors (funcall lang-f)))))
		        ;; if file contains she-bangs, then make it executable
		        (when she-bang
			  (unless tangle-mode (setq tangle-mode #o755)))
		        (when tangle-mode
			  (add-to-list 'modes (org-babel-interpret-file-mode tangle-mode)))
		        ;; Possibly create the parent directories for file.
		        (let ((m (funcall get-spec :mkdirp)))
			  (and m fnd (not (string= m "no"))
			       (setq make-dir t)))
		        ;; Handle :padlines unless first line in file
		        (unless (or (string= "no" (funcall get-spec :padline))
				    (= (point) (point-min)))
			  (insert "\n"))
		        (when (and she-bang (not she-banged))
			  (insert (concat she-bang "\n"))
			  (setq she-banged t))
		        (org-babel-spec-to-string spec)))
		    lspecs)
		   (when make-dir
		     (make-directory fnd 'parents))
                   ;; erase previous file
                   (when (file-exists-p file-name)
                     (delete-file file-name))
		   (write-region nil nil file-name)
		   (mapc (lambda (mode) (set-file-modes file-name mode)) modes)
                   (push file-name path-collector))))))
	 (if (equal arg '(4))
	     (aprog1 (fakemake--tangle-single-block 1 t)
               (setq block-counter (if it 1 0)))
	   (cl-multiple-value-bind (blocks counter) (fakemake--tangle-collect-blocks/mv
                                                     lang-re tangle-file)
             (setq block-counter counter)
             blocks)))
	(message "Tangled %d code block%s from %s" block-counter
		 (if (= block-counter 1) "" "s")
		 (file-relative-name (buffer-file-name
		                      (or (buffer-base-buffer)
                                          (current-buffer)
                                          (and (org-src-edit-buffer-p)
                                               (org-src-source-buffer))))
                                     fakemake-project-root))
	;; run `org-babel-post-tangle-hook' in all tangled files
	(when org-babel-post-tangle-hook
	  (mapc
	   (lambda (file)
	     (org-babel-with-temp-filebuffer file
	       (run-hooks 'org-babel-post-tangle-hook)))
	   path-collector))
	path-collector))))

(defun fakemake--tangle-file (file &optional target-file lang-re)
  "Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.

Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.

Optional argument LANG-RE can be used to limit the exported
source code blocks by languages matching a regular expression.

Return a list whose CAR is the tangled file name."
  (interactive "fFile to tangle: \nP")
  (let* ((visited (find-buffer-visiting file))
         (buffer (or visited (find-file-noselect file))))
    (prog1
        (with-current-buffer buffer
          (org-with-wide-buffer
           (mapcar #'expand-file-name
                   (fakemake--tangle nil target-file lang-re))))
      (unless visited (kill-buffer buffer)))))

(defmacro do-file-names-as-in-org-babel-tangle (&rest body)
  (declare (indent 0))
  ;; There is no interface to individual tangle operations
  ;; so we just copy the code the way it is in Emacs now.
  ;; For different versions of Emacs, different code should be used here.
  ;;
  ;; Instead of this,
  ;; Org should simply allow to append to a file when tangling
  ;; or, if that's forbidden to it by its gods,
  ;; have (lambda (by-fn) (let ((file-name (car by-fn))) ..) ..)
  ;; that's called on individual file names, as a named function
  ;; so that we can advise it rather than org-babel-tangle,
  ;; as we do below.

  ;; Still better is for org-babel-tangle to be a generic function
  (let ((by-fn (make-symbol "by-fn")))
    `(save-restriction
       (save-excursion
         (let ((org-babel-default-header-args
	        (if target-file
		    (org-babel-merge-params org-babel-default-header-args
					    (list (cons :tangle target-file)))
	          org-babel-default-header-args))
	       (tangle-file
	        (when (equal arg '(16))
	          (or (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))
		      (user-error "Point is not in a source code block")))))
           (dolist ( ,by-fn
                     (if (equal arg '(4))
	                 (fakemake--my-tangle-single-block 1 t)
	               (fakemake--tangle-collect-blocks lang-re tangle-file)))
             (when-let ((file-name (car ,by-fn)))
               ,@body)))))))

(fakemake-done-defvar fakemake-tangles nil
  "List of pairs (org-file . target-files)")
(defmacro with-org-babel-tangle-appending (&rest body)
  ;; Note: (run-hooks 'org-babel-pre-tangle-hook)
  ;; is only evaluated after the contents of the files are preserved.
  (declare (indent 0))
  (let ((preserved-alist (make-symbol "preserved-alist")))
    `(let (,preserved-alist)
       (cl-flet ((preserve (&optional arg target-file lang-re)
                   (ignore lang-re)
                   ;; target-file and lang-re are used implicitly
                   ;; in do-file-names-as-in-org-babel-tangle
                   (do-file-names-as-in-org-babel-tangle
                     (cl-pushnew (file-relative-name file-name
                                                     fakemake-project-root)
                                 (alist-get (file-relative-name
                                             (buffer-file-name)
                                             fakemake-project-root)
                                            fakemake-tangles
                                            nil nil #'string-equal)
                                 ;; This is a problem, as we can confuse
                                 ;; the nil symbol and "nil" filename.
                                 :test #'string-equal)
                     (with-current-buffer
                         (or (alist-get file-name
                                        ,preserved-alist
                                        nil nil
                                        #'string-equal)
                             (let ((temp-buffer (generate-new-buffer
                                                 " *temp*")))
                               (push (cons file-name temp-buffer)
                                     ,preserved-alist)
                               temp-buffer))
                       (when (file-exists-p file-name)
                         (insert-file-contents-literally file-name)))))
                 (restore (&optional arg target-file lang-re)
                   (ignore lang-re)
                   ;; target-file and lang-re are used implicitly
                   ;; in do-file-names-as-in-org-babel-tangle
                   (do-file-names-as-in-org-babel-tangle
                     (when (file-exists-p file-name)
                       (with-current-buffer (find-file-noselect file-name)
                         (save-excursion
                           (goto-char (point-min))
                           (insert-buffer-substring
                            (or (alist-get file-name
                                           ,preserved-alist
                                           nil nil
                                           #'string-equal)
                                (error "fakemake: New file name to tangle to appeared while tangling"))))
                         (save-buffer) (kill-buffer))))))
         (advice-add 'fakemake--tangle :before #'preserve)
         (advice-add 'fakemake--tangle :after #'restore)
         (unwind-protect (cl-locally ,@body)
           (advice-remove 'fakemake--tangle #'restore)
           (advice-remove 'fakemake--tangle #'preserve)
           (mapc (lambda (x) (kill-buffer (cdr x))) ,preserved-alist))))))

(defmacro with-org-babel-set-up (&rest body)
  (declare (indent 0))
  `(let ((ob-flags use-flags))
     (with-org-links-redirection-to org-sources-directory
       (with-org-babel-tangle-appending ,@body))))

(defvar fakemake-package-metadata-written nil)
(defvar fakemake-maintainer nil)
(defvar fakemake-package-url nil
  "The home page of the package, if the feature represents such.")
(defvar fakemake-feature-repository nil
  "The repository where the feature is stored, without trailing slash.

For example, “https://framagit.org/akater”.")
(defvar fakemake-feature-repository-ecosystem-prefix nil
  "A prefix to distingush the ecosystem / language used for the feature.

For example, “elisp”.")
(declare-function lm-version "lisp-mnt")

(defvar fakemake-decorate-regexps-blacklist nil)
(defun fakemake-decorate (tangled-file &optional for-elpa)
  "Post-process elisp-source file TANGLED-FILE."
  (unless (cl-member (file-relative-name tangled-file fakemake-project-root)
                     fakemake-decorate-regexps-blacklist
                     :test #'string-matched-p)
    (insert-elisp-preamble
     tangled-file nil
     (when (and for-elpa
                (string-equal (file-relative-name tangled-file
                                                  fakemake-project-root)
                              (format "%s.el" fakemake-feature)))
       (when fakemake-package-metadata-written
         (error "Metadata was already generated once"))
       (setf fakemake-package-metadata-written t)
       (package-desc-create
        :name fakemake-feature
        :version fakemake-package-version
        :summary nil
        :reqs
        (mapcar
         (lambda (s)
           (cl-etypecase s
             (cons s)
             (symbol
              (list s
                    (cl-case s
                      (emacs
                       emacs-version)
                      (otherwise
                       (with-current-buffer (find-file-noselect
                                             (locate-library
                                              (concat (symbol-name s) ".el")))
                         (aprog1 (lm-version)
                           (unless (stringp it)
                             (error
                              "Can't determine version of dependency `%s'"
                              s))))))))))
         fakemake-package-requires)
        :kind
        ;; kind needs to be set
        nil
        :archive nil
        :dir nil
        :extras
        `((:authors
           ,(cl-etypecase fakemake-authors
              (list fakemake-authors)
              (string
               (cons fakemake-authors
                     (or (stringp+ fakemake-author-email)
                         (error "Single author but no email provided; please set `%s'"
                                'fakemake-author-email))))))
          (:maintainer
           ,@(cl-etypecase fakemake-maintainer
               (null
                (cons (or (stringp+ fakemake-authors)
                          (error "Maintainer is not specified but no single author to presume a maintainer."))
                      (or (stringp+ fakemake-author-email)
                          (error "Single author but no email provided; please set `%s'"
                                 'fakemake-author-email))))
               (cons (unless (and (stringp (car fakemake-maintainer))
                                  (stringp (cdr fakemake-maintainer)))
                       (error "Maintainer should be specified as (author . email), both strings, but it's not"))
                     fakemake-maintainer)
               (string
                (cons fakemake-maintainer
                      (or (stringp+ fakemake-maintainer-email)
                          (error "A maintainer's name is specified but no email provided; please set `%s' or use a cons value for `%s'"
                                 'fakemake-maintainer-email
                                 'fakemake-maintainer))))))
          (:url .
                ,(or fakemake-package-url
                     (when (and fakemake-feature-repository
                                fakemake-feature-repository-language-prefix)
                       (concat fakemake-feature-repository "/"
                               (aif fakemake-feature-repository-language-prefix
                                   (concat it "-")
                                 "")
                               fakemake-feature)))))
        :signed nil)))
    (insert-elisp-postamble tangled-file)))

(defun fakemake-prepare (&optional for-elpa)
  (let* (tangle-index
         (all-tangled-source-files
          (let (all-tangled-source-files)
            (dolist (file fakemake-org-files-in-order
                          (nreverse all-tangled-source-files))
              (dolist (tangled (let ((org-file (concat file ".org")))
                                 (if (file-exists-p org-file)
                                     ;; (with-org-tangle-session tangle-index
                                     ;;  (org-babel-tangle-file org-file))
                                     (with-org-babel-set-up
                                      (fakemake--tangle-file org-file))
                                   (fakemake-message "Skipping non-existing org file %s"
                                                     org-file)
                                   nil)))
                (setq tangled (file-relative-name tangled
                                                  fakemake-project-root))
                ;; TODO: if the file in question is in org-files-for-testing
                ;; record its outputs as source-files-for-testing
                (when (and
                       (not (string-match-p (rx string-start "etc/") tangled))
                       (string-match-p (rx ".el" string-end) tangled))
                  ;; TODO: We probably better record files of other types too
                  (cl-pushnew tangled all-tangled-source-files
                              :test #'string-equal)))))))
    (setq fakemake-elisp-source-files-in-order
          ;; these won't really be “in order”
          ;; TODO: Do something about this

          ;; We want a copy, always
          ;; but cl-remove will only do one if necessary
          ;; so we do it manually:
          (let ((tail all-tangled-source-files) copy)
            (while tail
              (let ((elt (pop tail)))
                (unless (or (string-match-p (rx string-start "lisp/") elt)
                            (fakemake--file-from-elisp-source-directory-p
                             elt))
                  (push elt copy))))
            (nreverse copy)))
    (dolist (tangled-file all-tangled-source-files)
      (unless (aand (file-name-directory tangled-file)
                    (string-equal "site-gentoo.d" it))
        (fakemake-decorate tangled-file for-elpa))))
  (dolist (p fakemake-tangles) (nreversef (cdr p)))
  ;; For more consistent intermediate buildenvs,
  (fakemake-process-special-dirs)
  nil)

(defun fakemake-config ()
  ;; when site-lisp-configuration is absent,
  ;; any config beyond in-source autoloads is discarded
  (when autoloads-file
    ;; I generally do not `require' in defuns
    ;; but this file is one-time init
    (require 'autoload)
    (cond
     (site-autoloads
      (let ((generated-autoload-file (expand-file-name
                                      (if site-autoloads
                                          (format "site-gentoo.d/%s-gentoo.el"
                                                  fakemake-feature)
                                        autoloads-file))))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (ensure-directory default-directory)
          (save-buffer)                 ; file should exist
          (if (not site-autoloads) (progn (kill-buffer)
                                          (update-directory-autoloads
                                           default-directory))
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert ?\n
                      (format ";;; %s site-lisp configuration"
                              fakemake-feature)
                      ?\n ?\n)
              (prin1 `(add-to-list 'load-path ,(expand-file-name
                                                (ensure-string fakemake-feature)
                                                sitelisp))
                     (current-buffer))
              (insert ?\n ?\n)
              (goto-char (point-max))
              (insert ?\n
                      (format ";;; begin: forms written by `%s'"
                              'autoload-generate-file-autoloads)
                      ?\n)
              (save-buffer)
              (dolist (default-directory fakemake-elisp-source-directories)
                (setq default-directory
                      (expand-file-name default-directory fakemake-project-root))
                (do-default-directory-files ((el-file "elisp file")
                                             (rx ".el" string-end) t t)
                  "Generated autoloads from %s file%s"
                  ;; todo: deal with the case when el-file
                  ;; specifies generated-autoload-file
                  (let (exception)
                    (cond
                     ((setq exception
                            (cl-member (file-relative-name el-file fakemake-project-root)
                                       fakemake-install-regexps-blacklist
                                       :test #'string-matched-p))
                      (do-not-count "as blacklisted by %s in %s"
                                    (car exception)
                                    'fakemake-install-regexps-blacklist))
                     ((setq exception
                            (cl-member (file-relative-name el-file fakemake-project-root)
                                       fakemake-autoloads-regexps-blacklist
                                       :test #'string-matched-p))
                      (do-not-count "as blacklisted by %s in %s"
                                    (car exception)
                                    'fakemake-autoloads-regexps-blacklist))
                     (t
                      (insert ?\n)
                      (let ((generated-autoload-load-name (file-name-base el-file)))
                        (autoload-generate-file-autoloads el-file (current-buffer))))))))
              (insert ?\n
                      (format ";;; end: forms written by `%s'"
                              'autoload-generate-file-autoloads)
                      ?\n))
            (save-buffer) (kill-buffer)))))
     ((and autoloads-file
           (aand (aand1 (consp+ fakemake-elisp-source-directories)
                        (null (cdr it)))
                 (stringp+ (car it))
                 (string-equal it fakemake-project-root)))
      (let ((generated-autoload-file (expand-file-name
                                      (or (stringp+ autoloads-file)
                                          (format "%s-autoloads.el"
                                                  fakemake-feature))
                                      fakemake-project-root)))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (save-buffer)                 ; file should exist
          (kill-buffer)
          (update-directory-autoloads fakemake-project-root))
        ;; or maybe just
        ;; (update-directory-autoloads fakemake-project-root)
        ))
     (t
      ;; in-source, complicated
      (defvar autoload-modified-buffers)
      (dolist (default-directory fakemake-elisp-source-directories)
        (setq default-directory
              (expand-file-name default-directory fakemake-project-root))
        (aprog1 (let ((generated-autoload-file (if (stringp autoloads-file)
                                                   (expand-file-name
                                                    autoloads-file
                                                    fakemake-project-root)
                                                 (format "%s-%s-autoloads.el"
                                                         fakemake-feature
                                                         (cl-substitute
                                                          ?- ?/
                                                          (file-relative-name
                                                           default-directory
                                                           fakemake-project-root)
                                                          :test #'char-equal))))
                      autoload-modified-buffers)
                  ;; Maybe we could just call update-directory-autoloads
                  ;; but I don't have time for this right now.
                  (let ((buffer (with-current-buffer (find-file-noselect
                                                      generated-autoload-file)
                                  ;; Sometimes people may tangle to -autoloads on their own
                                  (goto-char (point-max))
                                  (save-buffer)
                                  (current-buffer))))
                    (do-default-directory-files ((el-file "elisp file")
                                                 (rx ".el" string-end) t t)
                      "Generated autoloads from %s file%s"
                      ;; todo: deal with the case when el-file
                      ;; specifies generated-autoload-file
                      (let (exception)
                        (cond
                         ((setq exception
                                (cl-member (file-relative-name el-file fakemake-project-root)
                                           fakemake-install-regexps-blacklist
                                           :test #'string-matched-p))
                          (do-not-count "as blacklisted by %s in %s"
                                        (car exception)
                                        'fakemake-install-regexps-blacklist))
                         ((setq exception
                                (cl-member (file-relative-name el-file fakemake-project-root)
                                           fakemake-autoloads-regexps-blacklist
                                           :test #'string-matched-p))
                          (do-not-count "as blacklisted by %s in %s"
                                        (car exception)
                                        'fakemake-autoloads-regexps-blacklist))
                         (t
                          (insert ?\n)
                          (let ((generated-autoload-load-name (file-name-base el-file)))
                            (autoload-generate-file-autoloads el-file))))))
                    (with-current-buffer buffer
                      (save-buffer) (aprog1 (file-relative-name (buffer-file-name)
                                                                fakemake-project-root)
                                      (kill-buffer))))
                  )
          ;; I used to push generated-autoload-file
          ;; but I just don't trust this shit
          (push it fakemake-files-to-install-into-lispdir)
          (push it fakemake-autoload-files))))))
  nil)

(defmacro with-el-files-from-elisp-source-directories-included-into ( var
                                                                      &rest
                                                                      body)
  (declare (indent 1))
  (cl-check-type var symbol)
  (if (eq 'el-files-from-elisp-source-directories var)
      ;; User shouldn't use this variable;
      ;; we probably better warn but I'm not in the mood right now.
      `(cl-locally ,@body)
    `(let ((,var
            (nconc
             (let ((el-files-from-elisp-source-directories
                    (mapcan (lambda (dir)
                              (let ((names
                                     (directory-files dir nil
                                                      (rx ".el" string-end))))
                                (let ((tail names))
                                  (while tail
                                    (setf (car tail)
                                          (file-relative-name
                                           (expand-file-name (car tail) dir)
                                           fakemake-project-root))
                                    (pop tail)))
                                names))
                            (setq fakemake-elisp-source-directories
                                  (sort fakemake-elisp-source-directories
                                        (lambda (x y)
                                          (aif (aand1 (cl-plusp+
                                                       (cl-mismatch x y :test #'char-equal))
                                                      (char-equal ?/ (aref x (1- it))))
                                              (cond
                                               ((= it (length x)) nil)
                                               ((= it (length y)) t)
                                               (t
                                                (string-lessp x y)
                                                ;; (string-lessp (substring x it)
                                                ;;               (substring y it))
                                                ))
                                            (string-lessp x y))))))))
               (when el-files-from-elisp-source-directories
                 (cl-flet ((files-equal (x y)
                             (string-equal
                              (file-relative-name x fakemake-project-root)
                              (file-relative-name y fakemake-project-root))))
                   (while (and
                           el-files-from-elisp-source-directories
                           (cl-member (car
                                       el-files-from-elisp-source-directories)
                                      ,var
                                      :test #'files-equal))
                     (warn "Duplicated file scheduled for compilation: %s"
                           ;; This should probably be style-warning
                           ;; but Elisp doesn't have those anyway.
                           (pop el-files-from-elisp-source-directories)))
                   (let ((tail el-files-from-elisp-source-directories))
                     (while (cdr tail)
                       (if (cl-member (cadr tail)
                                      ,var
                                      :test #'files-equal)
                           (warn "Duplicated file scheduled for compilation: %s"
                                 (pop (cdr tail)))
                         (pop tail))))))
               el-files-from-elisp-source-directories)
             ,var)))
       ,@body)))

(defvar fakemake-compile-error nil)

(defvar fakemake-stop-after-first-error t)

(fakemake-done-defvar fakemake--compiled nil
  "Alist mapping source files to compiled files")
(defun fakemake--compiledp (el-file)
  (alist-get el-file fakemake--compiled nil nil #'string-equal))
(defun fakemake-compile ()
  (fakemake-process-special-dirs)
  (let (compile-errors)
    (or (with-el-files-from-elisp-source-directories-included-into
            fakemake-elisp-source-files-in-order
          "All files in elisp-source directories should be compiled"
          (do-files-list ((el-file "elisp file")
                          fakemake-elisp-source-files-in-order
                          compile-errors)
            "Compiled %s file%s"
            ;; el-file should be relative to project root
            ;; in elisp-source-files-in-order already
            ;; for now we obsessively normalize
            ;; but this is wrong
            (let (exception)
              (cond
               ((string-prefix-p "fakemake/"
                                 (file-relative-name
                                  el-file fakemake-project-root))
                ;; TODO: this should happen silently, we only leave it for debug
                (do-not-count "as element of %s" "fakemake/"))
               ;; there better be the following option:
               ;; if the file was tangled but was not decorated,
               ;; do not compile it either
               ((setq exception
                      (cl-member (file-relative-name
                                  el-file fakemake-project-root)
                                 fakemake-compile-regexps-blacklist
                                 :test #'string-matched-p))
                (do-not-count "as blacklisted by %s" (car exception)))
               ((string-equal autoloads-file (file-name-base el-file))
                (do-not-count))
               ((let ((load-path (cons (expand-file-name
                                        (or (file-name-directory
                                             (file-relative-name
                                              el-file fakemake-project-root))
                                            ".")
                                        fakemake-project-root)
                                       load-path)))
                  ;; we should probably remove ./fakemake from load-path
                  (byte-compile-file
                   ;; how do we collect compile errors?
                   el-file))
                (awhen (file-exists-p+
                        ;; File might have no-byte-compile set
                        ;; in which case byte-compile-file
                        ;; will nevertheless report success.
                        (byte-compile-dest-file
                         ;; It would be much better if we could get
                         ;; the actual name of the compiled file
                         ;; directly.
                         el-file))
                  (let ((compiled (file-relative-name
                                   it fakemake-project-root)))
                    (aif (cl-assoc el-file fakemake--compiled
                                   :test #'string-equal)
                        (error "Two source files with the same compile target %s: %s, %s"
                               compiled (cdr it) el-file)
                      (push (cons el-file compiled)
                            fakemake--compiled))
                    (cl-pushnew
                     compiled
                     fakemake-files-to-install-into-lispdir
                     :test #'string-equal)))
                (cl-pushnew
                 (file-relative-name el-file fakemake-project-root)
                 fakemake-files-to-install-into-lispdir
                 :test #'string-equal)
                ;; TODO: if the file in question is in
                ;; source-files-for-testing record its outputs as
                ;; code-files-for-testing
                )
               (t
                (do-not-count)
                (setq compile-errors t)
                (when fakemake-stop-after-first-error
                  (cl-return compile-errors)))))))
        (fakemake-config))
    compile-errors))

(declare-function 'ort-tests-in-file "ort")
(defun fakemake--elc-file-p (x)
  (string-suffix-p ".elc" x))
(defun fakemake--el-file-p (x)
  (string-suffix-p ".el" x))
(defun fakemake--test/ort (org-files-in-order &optional keep-going)
  (require 'ort)
  (let (failed-tests)
    (cl-dolist (x org-files-in-order failed-tests)
      (setq x (concat x ".org"))
      (message "Trying to run ort tests in %s" x)
      ;; Here, we should load all files tangled from x
      (dolist (y (alist-get x fakemake-tangles nil nil #'string-equal))
        (cond
         ((awhen1 (fakemake--elc-file-p y)
            (setq y it))
          (message "To test %s, loading %s" x y)
          (load y nil t))
         ((fakemake--el-file-p y)
          (load (or (awhen1 (fakemake--compiledp y)
                      (message "To test %s, loading %s" x it))
                    (progn
                      (message "To test %s, we tried loading compiled code, but found none; loading source %s." x y)
                      y))
                nil t))))
      (condition-case e (awhen (let (org-confirm-babel-evaluate)
                                 (ort-run-tests-in-file x
                                   :stop-on-first-failed (not keep-going)
                                   :save-buffer t))
                          (if keep-going (push (cons x it) failed-tests)
                            (cl-return it)))
        (ort-no-tests
         (message "ORT found no tests in %s"
                  (file-relative-name (cadr e)
                                      fakemake-project-root)))))))

(defun fakemake-test ()
  (cond
   ;; (user-specified-procedure)
   ;; (user-specified-test-framework)
   ((bound-and-true-p fakemake-files-to-test-via-ort)
    (fakemake-message "Testing via ORT, as specified by %s"
                      fakemake-feature)
    (fakemake--test/ort fakemake-files-to-test-via-ort
                        fakemake-ort-keep-going))
   ((progn
      (fakemake-message "Trying to test using %s'%s own testing facility"
                        fakemake-feature (if (string-suffix-p
                                              "s" (symbol-name
                                                   fakemake-feature))
                                             ""
                                           "s"))
      (bound-and-true-p fakemake-org-files-for-testing-in-order))
    (let (per-file-tests)
      (dolist (x fakemake-files-to-load-for-testing-in-order)
        (load x nil t)
        ;; TODO: if the above run some ert functions, do not guess anything else
        (let ((run-tests-symbol (ifmt "%s-run" (file-name-base x))))
          (if (fboundp run-tests-symbol)
              (push (cons run-tests-symbol
                          (funcall (symbol-function run-tests-symbol)))
                    per-file-tests)
            (message "No #'%s found in %s" run-tests-symbol x))))
      (let ((run-tests-symbol (ifmt "%s-tests-run" fakemake-feature)))
        (if (fboundp run-tests-symbol)
            (unless (assq run-tests-symbol per-file-tests)
              (push (cons run-tests-symbol
                          (funcall (symbol-function run-tests-symbol)))
                    per-file-tests))
          (message "No #'%s found" run-tests-symbol)))
      (cl-delete-if #'null per-file-tests :key #'cdr)))
   ((progn
      (message "%s provides no org files for testing" fakemake-feature)
      nil))
   ((progn
      (message "%s provides no testing code we know of" fakemake-feature)
      fakemake-test-try-ort)
    (fakemake-message "Trying to run ORT tests on our own")
    (fakemake--test/ort fakemake-org-files-in-order
                        (or fakemake-ort-keep-going
                            (eq 'keep-going fakemake-test-try-ort))))
   ;; (looks-like-test-framework-in-repo)
   (t (error "Don't know how to test"))))

(defvar fakemake-test-phase-enabled t)

(defun fakemake (&optional target live)
  "Kill Emacs in the end (with appropriate error code) when LIVE is nil."
  (setq target (or target 'default))
  (let ((status
         (progn
           (setq backtrace-line-length 0)
           (cl-ecase target
             (default
               (fakemake 'compile live))
             (all
              (fakemake 'default t))
             (clean
              (fakemake-message "making target %s" target)
              (fakemake-clean))
             (elpa-release
              (fakemake-message "Making elpa release")
              (fakemake 'default)
              (fakemake-clean)
              (or (fakemake-prepare 'for-elpa)
                  (progn
                    (fakemake-done-cache-variables 'prepare)
                    nil)))
             (prepare
              (if-let ((prepared (fakemake-donep 'prepare)))
                  (progn (fakemake-message "%s done:" target)
                         (load prepared nil t)
                         nil)
                (fakemake-message "making target %s" target)
                (when fakemake-use-lice-p (require 'lice))
                (or (fakemake-prepare)
                    (progn
                      (fakemake-done-cache-variables 'prepare)
                      nil))))
             (compile
              (if-let ((compiled (fakemake-donep 'compile)))
                  (progn (fakemake-message "%s done:" target)
                         (load compiled nil t)
                         nil)
                (fakemake-message "making target %s" target)
                (fakemake 'prepare t)
                (or (fakemake-compile)
                    (progn (fakemake-done-cache-variables 'compile)
                           nil))))
             (test
              (when fakemake-test-phase-enabled
                (fakemake-check-done compile
                                     "Source had not been compiled.  Testing prior to compilation is not supported.")
                (fakemake-test)))))))
    ;; The remaining code is a remnant of times when we returned error sexps.
    ;; Nowadays we mostly just exit with debugger's backtrace.
    ;; This logic is nevertheless valid for “expected” errors
    ;; which can still be preserved as sexps
    ;; and returned by top-level `fakemake' calls.
    (cond
     ((null status)
      (fakemake-message "made target %s" target)
      status)
     ((progn
        (if (eq t status)
            (fakemake-message "target %s failed" target)
          (fakemake-message "target %s failed: %S" target status))
        live)
      status)
     (t
      (kill-emacs
       ;; we return 1 for every phase
       ;; because that's what batch-byte-compile-file returns when it errors
       1)))))
