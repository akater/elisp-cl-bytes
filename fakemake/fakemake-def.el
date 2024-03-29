;;;  -*- lexical-binding: t -*-

(defconst-with-prefix fakemake
  feature 'cl-bytes
  authors "Dima Akater" author-email "nuclearspace@gmail.com"
  package-requires '((emacs "24.3"))
  first-publication-year-as-string "2023"
  org-files-in-order '("cl-bytes")
  site-lisp-config-prefix "50"
  license "GPL-3")

(advice-add 'fakemake-test :before
            (lambda () (require 'org-src-elisp-extras)))
