;;;  -*- lexical-binding: t -*-

(defconst sitelisp
  (or (getenv "SITELISP")
      (let ((default-sitelisp "/usr/share/emacs/site-lisp/"))
        (warn "SITELISP is not set. Defaulting to %s" default-sitelisp)
        default-sitelisp)))

(defvar site-autoloads (file-directory-p+
                        (expand-file-name "site-gentoo.d" sitelisp)))

(defconst siteetc
  (file-name-as-directory
   (or (getenv "SITEETC")
       (let ((default-siteetc "/usr/share/emacs/etc/"))
         (warn "SITEETC is not set. Defaulting to %s" default-siteetc)
         default-siteetc))))

(defconst org-upstream-sources-directory
  (getenv "ORG_UPSTREAM_SOURCES_DIRECTORY")
  "Directory to install Org source files to.

Iff non-nil, Org source files will be installed into this directory.

If non-nil, Elisp source files will be linked to files in this directory
unless `org-local-sources-directory' is also non-nil.

Meant to be set by package maintainers.")

(defconst org-local-sources-directory (getenv "ORG_LOCAL_SOURCES_DIRECTORY")
  "Local directory with Org source files that is presumed to exist.

If non-nil, Elisp source files will be linked to files in this directory.

Meant to be set by contributors who keep a local copy of the
repository.")

(defvar use-flags (read-list (getenv "USE")))

(load "fakemake-def" nil t)

(defvar autoloads-file (format "%s-autoloads.el" fakemake-feature))

(defconst lispdir (file-name-as-directory
                   (or (getenv "LISPDIR")
                       (expand-file-name (concat (ensure-string fakemake-feature)
                                                 ;; because this is
                                                 ;; how it's done in Portage
                                                 "/")
                                         sitelisp))))

(defconst etcdir (file-name-as-directory
                   (or (getenv "ETCDIR")
                       (expand-file-name (concat (ensure-string fakemake-feature)
                                                 "/")
                                         siteetc))))

(defvar fakemake-feature-full-name nil)
(defvar fakemake-use-lice-p nil)

(defvar fakemake-compile-regexps-blacklist nil)
;; (defvar fakemake-compile-file-name-base-blacklist nil)
(defvar fakemake-autoloads-regexps-blacklist nil)
(defvar fakemake-install-regexps-blacklist nil)
;; (defvar fakemake-install-file-name-base-blacklist nil)

(defvar fakemake-test-try-ort t)
(defvar fakemake-ort-keep-going nil)

(defvar fakemake-project-root default-directory)
