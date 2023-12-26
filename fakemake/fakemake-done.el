;;; fakemake-done.el --- Interface for restoring intermediate buildenv in-between phases  -*- lexical-binding: t -*-

(defconst fakemake-done-directory (expand-file-name "fakemake"))

(defun fakemake-done-file-name-base (operation)
  (format "%s-done-%s" 'fakemake operation))

(defun fakemake-done-file-name-no-extension (operation)
  (expand-file-name (fakemake-done-file-name-base operation)
                    fakemake-done-directory))

(defun file-exists-p+ (filename)
  "When (FILE-EXISTS-P FILENAME) is non-nil, return FILENAME."
  (when (file-exists-p filename) filename))

(defun fakemake-donep (operation)
  (let ((fakemake-done-file-name-no-extension
         (fakemake-done-file-name-no-extension operation)))
    (or (file-exists-p+ (concat fakemake-done-file-name-no-extension ".elc"))
        (file-exists-p+ (concat fakemake-done-file-name-no-extension ".el")))))

(defmacro fakemake-check-done (operation &optional error-string &rest args)
  (declare (indent 1))
  (unless (memq operation '(compile test))
    (error "Unsupported operation: %s" operation))
  (let ((op (make-symbol (let ((name (symbol-name operation)))
                           (format (if (char-equal ?e
                                                   (aref name
                                                         (1- (length name))))
                                       "%sd"
                                     "%sed")
                                   name))))
        (success (make-symbol "success")))
    `(let ((,op (fakemake-donep ',operation)))
       (unless ,op
         (error ,(or error-string
                     (format "Phase “%s” had not been completed" operation))
                ,@args)))))

(defun fakemake-done-load-intermediate-buildenv (operation &optional file)
  ;; (cl-check-type file (or null string))
  (setq file (or file (fakemake-done-file-name-base operation)))
  (princ (format "Loading build environment from phase %s..." operation))
  ;; TODO: optionally support more verbose message:
  ;; (princ (format "Loading build environment %s ..." file))
  (let (success)
    (unwind-protect (progn (load file nil t)
                           (setq success t)
                           (princ " done")
                           ;; TODO: Make it print just enough newlines
                           (terpri))
      (unless success
        (terpri)
        (error "Can't load %s" file)))))

(defun fakemake-done-load-latest ()
  (let ((ops '( test compile
                ;; configure
                prepare))
        last-done file)
    (while ops
      (if (setq file (fakemake-donep (car ops)))
          (setq last-done (pop ops)
                ops nil)
        (pop ops)))
    (when last-done
      (fakemake-done-load-intermediate-buildenv last-done file))))

(fakemake-done-load-latest)

(defvar fakemake-done-cached-variables nil)

(cl-defmacro fakemake-done-defvar ( name
                                    &optional
                                    (initvalue nil initvalue-supplied-p)
                                    (docstring nil docstring-supplied-p))
  (declare (indent 1))
  `(prog1 (defvar ,name ,@(when initvalue-supplied-p `(,initvalue))
            ,@(when docstring-supplied-p `(,docstring)))
     ;; Unlike in CL, it's better without toplevel eval-when
     (cl-pushnew ',name fakemake-done-cached-variables :test #'eq)))

(defun fakemake-done-cache-variables (operation)
  (with-current-buffer (find-file-noselect
                        (concat (fakemake-done-file-name-no-extension
                                 operation)
                                ".el"))
    (erase-buffer)
    (let ((name (concat (fakemake-done-file-name-base operation) ".el"))
          (local-variables '(:lexical-binding t)))
      (apply #'elisp-insert-header (plist-with-keys name local-variables))
      (comment-region (line-beginning-position) (line-end-position))
      (terpri (current-buffer))
      (terpri (current-buffer)))
    (when-let ((previous-operation
                (cadr (memq operation '( test compile
                                         ;; configure
                                         prepare)))))
      (prin1 `(load ,(fakemake-done-file-name-base previous-operation)
                    nil t)
             (current-buffer))
      (terpri (current-buffer))
      (terpri (current-buffer)))
    ;; Only here does proper caching of variables start.
    (let ((tail fakemake-done-cached-variables))
      (while tail
        (let ((name (pop tail)))
          (prin1 `(defvar ,name) (current-buffer))
          (terpri (current-buffer))
          (prin1 `(setq ,name ',(symbol-value name)) (current-buffer))
          (terpri (current-buffer))
          (terpri (current-buffer)))))
    (save-buffer) (kill-buffer)))

(defvar fakemake-original-files
  (let ((current-level (directory-files default-directory nil
                                        (rx string-start
                                            (or (not ?.)
                                                (seq ?. (or (not ?.)
                                                            (seq ?.
                                                                 anything)))))
                                        t))
        files dirs)
    ;; One top-level exception
    ;; we don't want to check for it all the time
    ;; Note: I don't know how submodules work
    ;; so maybe there could be .git dirs deeper inside
    (if (not (string-equal ".git" (car current-level)))
        (let ((tail current-level))
          (while (cdr tail)
            (if (not (string-equal ".git" (cadr tail)))
                (pop tail)
              (npush-pop (cdr tail) dirs)
              (setf (car dirs)
                    (file-name-as-directory (car dirs)))
              (setq tail nil))))
      (npush-pop current-level dirs)
      (setf (car dirs)
            (file-name-as-directory (car dirs))))
    (while current-level
      (let ((file (pop current-level)))
        (cond
         ((file-symlink-p file)
          (push file files))
         ((file-directory-p file)
          (push (file-name-as-directory file) dirs)
          (let ((files (directory-files file nil
                                        (rx string-start
                                            (or (not ?.)
                                                (seq ?. (or (not ?.)
                                                            (seq ?.
                                                                 anything)))))
                                        t)))
            (while files
              (npush-pop files current-level)
              (setf (car current-level)
                    (concat (car dirs) (car current-level))))))
         (t
          (push file files)))))
    (nconc (nreverse dirs) files)))

(fakemake-done-defvar fakemake-original-files)

(fakemake-done-defvar fakemake-elisp-source-directories
  (list fakemake-project-root))

(fakemake-done-defvar use-flags)
(fakemake-done-defvar fakemake-compile-regexps-blacklist)
(fakemake-done-defvar fakemake-install-regexps-blacklist)
