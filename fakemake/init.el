;;;  -*- lexical-binding: t -*-

(defsubst file-directory-p+ (filename)
  "When (FILE-DIRECTORY-P FILENAME) is non-nil, return FILENAME."
  (when (file-directory-p filename) filename))

;; file-exists-p+ is defined in fakemake-done

(defsubst stringp+ (object)
  "When (STRINGP OBJECT) is non-nil, return OBJECT."
  (when (stringp object) object))

(defsubst functionp+ (object)
  "When (FUNCTIONP OBJECT) is non-nil, return OBJECT."
  (when (functionp object) object))

(defsubst cl-plusp+ (number)
  "When (CL-PLUSP NUMBER) is non-nil, return NUMBER."
  (when (cl-plusp number) number))

(require 'cl-macs)

(defsubst read-list (string-or-nil)
  (when string-or-nil
    (with-temp-buffer
      (insert ?\( string-or-nil ?\))
      (goto-char (point-min))
      (read (current-buffer)))))

(defsubst ifmt (s &rest objects) (intern (apply #'format s objects)))

(when (< emacs-major-version 28)
  (cl-deftype keyword () `(satisfies keywordp)))

(defsubst ensure-string (x)
  (cl-etypecase x
    (string x)
    (keyword (substring (symbol-name x) 1))
    (symbol (symbol-name x))
    (number (number-to-string x))))

(defsubst keyword (symbol-or-string)
  "Convert SYMBOL-OR-STRING to keyword."
  (cl-etypecase symbol-or-string
    (keyword symbol-or-string)
    ((or symbol string) (ifmt ":%s" symbol-or-string))))

(defmacro nreversef (place)
  `(setf ,place (nreverse ,place)))

(defmacro aprog1 (first &rest body)
  "Like `prog1', but the result of evaluating FIRST is bound to `it'.

The variable `it' is available within BODY.

FIRST and BODY are otherwise as documented for `prog1'."
  (declare (indent 1) (debug t))
  `(let ((it ,first))
     ,@body
     it))

(defmacro aif (test-form then-form &rest else-forms)
  (declare (indent 2))
  (let ((once-only (gensym "test-form-")))
    `(let ((,once-only ,test-form))
       (if ,once-only (let ((it ,once-only))
                        ;; (declare (type (not null) it))
                        ,then-form)
         (cl-locally ,@else-forms)))))

(defmacro awhen (test-form &rest then-forms)
  (declare (indent 1))
  `(let ((it ,test-form))
     (when it
       (cl-locally
        ;; (declare (type (not null) it))
        ,@then-forms))))

(when (< emacs-major-version 28)
  (defsubst ensure-list (object)
    "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
    (if (listp object) object
      (list object))))

(defmacro plist-with-keys (&rest symbols)
  (cons 'list (cl-loop for s in symbols collect (keyword s) collect s)))

(defmacro npush-pop (source target)
  "Like (push (pop SOURCE) TARGET) but reuse the cons cell, and return nil."
  `(cl-rotatef ,source (cdr ,source) ,target))

(defmacro aand (&rest conditions)
  "Like `and', but the result of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

CONDITIONS are otherwise as documented for `and'.

Note that some implementations of this macro bind only the first
condition to `it', rather than each successive condition."
  (declare (debug t))
  (cond ((null conditions)
         t)
        ((null (cdr conditions))
         (car conditions))
        (t
         `(awhen ,(car conditions)
            (aand ,@(cdr conditions))))))

(defmacro aand1 (x &rest xs)
    "Like AAND but returns the value of X for true.

The first argument X is thus mandatory."
    (if xs `(let ((it ,x)) (when (aand it ,@xs) it))
      x))

(defsubst fold (f x list) (while list (setq x (funcall f x (pop list)))) x)

(defmacro fif (test f x)
  "If TEST evaluates to non-nil, return (F X).  Otherwise, return X."
  (declare (indent 1))
  `(if ,test (funcall ,f ,x) ,x))

(defmacro when1 (test &rest body)
  (declare (indent 1))
  (let ((it (make-symbol "it")))
    `(let ((,it ,test))
       (when ,it ,@body ,it))))

(defmacro awhen1 (test &rest body)
  (declare (indent 1))
  `(let ((it ,test))
     (when1 it ,@body)))

(cl-defsubst string-contained-p (string list &key key)
  (cl-member string list :test #'string-equal :key key))

(defsubst string-matched-p (x y) (string-match-p y x))

(cl-defmacro do-files-list ((var list &optional return)
                            ;; (var list &key return (if-not-exists 'message))
                            &body body)
  "Evaluate BODY for each VAR in LIST, presuming VAR to be relative file name.

If BODY starts with a string, it is presumed to be format strings with two %s slots: length, plural/singular -s

VAR can be (var description); description will be evaluated each time
`do-not-count' is called, and if there's format-string, in the end too.

The arglist of do-not-count is (&optional format-string args)."
  (declare (indent 1))
  (let (description)
    (cl-assert (or (symbolp var) (prog1 (and (consp var)
                                             (prog1 (consp (cdr var))
                                               (setq description (cadr var)))
                                             (null (cddr var)))
                                   (setq var (car var)))))
    (let* ((do-not-count-g (gensym "do-not-count-"))
           (format-string (stringp+ (car body)))
           (count-g (when format-string (gensym "count-")))
           (body (fif format-string #'cdr body)))
      (setq description (or description "file"))
      (fif format-string
        (lambda (form)
          `(let ((,count-g 0))
             (prog1 ,form
               (message ,format-string
                        ,count-g (if (= 1 ,count-g) "" "s")))))
        `(cl-dolist (,var ,list ,return)
           ,(fif format-string (lambda (form)
                                 `(let (,do-not-count-g)
                                    ,form
                                    (unless ,do-not-count-g
                                      (cl-incf ,count-g))))
                 `(cl-flet (,(let ((local-format-string
                                    (gensym "format-string-"))
                                   (args (gensym "args-")))
                               `(do-not-count ( &optional ,local-format-string
                                                &rest ,args)
                                 (when ,local-format-string
                                   (cl-check-type ,local-format-string string)
                                   (apply #'message
                                          (concat
                                           "Skipping "
                                           ,(if (macroexp-const-p description)
                                                (eval `(ensure-string ,description)
                                                      t)
                                              `(ensure-string ,description))
                                           (if (and (cl-plusp
                                                     (length ,local-format-string))
                                                    (not
                                                     (or (char-equal
                                                          ?\s
                                                          (aref ,local-format-string 0))
                                                         (char-equal
                                                          ?\t
                                                          (aref ,local-format-string 0)))))
                                               " %s "
                                             " %s")
                                           ,local-format-string)
                                          ,var ,args))
                                 ,@(when format-string
                                     `((setq ,do-not-count-g t)))
                                 nil)))
                    (cond
                     ((not (file-exists-p ,var))
                      (message (concat "Skipping non-existing "
                                       (ensure-string ,description)
                                       " %s")
                               ,var))
                     (t ,@body)))))))))

(cl-defmacro do-default-directory-files ((var match
                                              &optional full nosort return)
                                         &body body)
  "Evaluate BODY for each file in default-directory matching MATCH.

VAR is bound to corresponding relative file name.

FULL, NOSORT work as in `directory-files'.

RETURN works as in `dolist'.

Macroexpands to do-files-list."
  (declare (indent 1))
  `(do-files-list (,var (directory-files default-directory
                                         ,full ,match ,nosort)
                        ,return)
     ,@body))

(cl-defmacro do-files-recursively ((var root &key do-not-descend return)
                                   &body body)
  (declare (indent 1))
  (let ((files (gensym "files-"))
        (o-o-do-not-descend (gensym "do-not-descend-"))
        (more-files (gensym "more-files-")))
    `(let ((,files (fold (lambda (list file)
                           (cl-delete file list :test #'string-equal))
                         (directory-files ,root nil nil t)
                         '("." "..")))
           (,o-o-do-not-descend ,do-not-descend))
       ,@(cl-psetf do-not-descend o-o-do-not-descend)
       (while ,files
         (let ((,var (pop ,files)))
           ;; Actually, declarations should go here
           ;; but we probably should not worry about this.
           (cond
            ((file-symlink-p ,var))
            ((file-directory-p ,var)
             (setq ,var (file-name-as-directory ,var))
             (unless (string-contained-p ,var ,do-not-descend)
               (let ((,more-files
                      (directory-files ,var nil
                                       (rx string-start
                                           (or (not ?.)
                                               (seq ?. (or (not ?.)
                                                           (seq ?.
                                                                anything)))))
                                       t)))
                 (while ,more-files
                   (npush-pop ,more-files ,files)
                   (setf (car ,files) (concat ,var (car ,files))))))))
           ,@body))
       ,return)))

(defsubst ensure-directory (dir) (make-directory dir t) dir)

(cl-defmacro with-filenames-relative-to (root names &body body)
  (declare (indent 2))
  (let ((root-g (gensym "root-")))
    `(let ((,root-g ,root))
       (cl-flet ((file-name-relative (filename)
                   (expand-file-name (replace-regexp-in-string
                                      (rx string-start (one-or-more ?/)) ""
                                      filename)
                                     ,root-g)))
         (let ,(mapcar (lambda (x) `(,x (when ,x (file-name-relative ,x))))
                       names)
           ,@body)))))

(defmacro defconst-with-prefix (prefix &rest pairs)
  (declare (indent 1))
  (let ((prefix-name (ensure-string prefix)))
    `(cl-eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(cl-loop for (key value) on pairs by #'cddr
                  collect `(defconst ,(ifmt "%s-%s"
                                            prefix-name (ensure-string key))
                             ,value)))))
