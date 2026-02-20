;;;; check-no-test-packages-in-src.lisp
;;;;
;;;; Fail when src/ contains direct references to test-only packages.

(require :asdf)

(defun %trim-string (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun %project-root ()
  (let* ((out (uiop:run-program '("git" "rev-parse" "--show-toplevel")
                                :output :string
                                :ignore-error-status t))
         (root (%trim-string (or out ""))))
    (if (plusp (length root))
        (uiop:ensure-directory-pathname root)
        (uiop:ensure-directory-pathname (uiop:getcwd)))))

(defparameter *forbidden-prefixes*
  '("it.bese.fiveam:" "fiveam:")
  "Package prefixes forbidden under src/.")

(defun %src-files (src-dir)
  (labels ((walk (dir)
             (append
              (remove-if-not (lambda (p)
                               (string-equal (or (pathname-type p) "") "lisp"))
                             (uiop:directory-files dir))
              (mapcan #'walk (uiop:subdirectories dir)))))
    (walk src-dir)))

(defun %line-has-forbidden-prefix-p (line)
  (let ((lower (string-downcase line)))
    (some (lambda (prefix)
            (search prefix lower :test #'char=))
          *forbidden-prefixes*)))

(defun %collect-violations (file)
  (with-open-file (in file :direction :input)
    (loop for line = (read-line in nil nil)
          for line-no from 1
          while line
          when (%line-has-forbidden-prefix-p line)
            collect (list file line-no line))))

(defun main ()
  (let* ((root (%project-root))
         (src (merge-pathnames "src/" root)))
    (unless (uiop:directory-exists-p src)
      (format *error-output* "[check] src directory not found: ~a~%" src)
      (uiop:quit 1))
    (let ((violations
           (mapcan #'%collect-violations (%src-files src))))
      (if violations
          (progn
            (format *error-output*
                    "[check] Found direct test package references under src/:~%")
            (dolist (v violations)
              (destructuring-bind (file line-no line) v
                (format *error-output* "~a:~d: ~a~%"
                        (uiop:native-namestring file)
                        line-no
                        line)))
            (format *error-output*
                    "~%Use runtime resolution (find-package/find-symbol/progv) instead of package-qualified symbols.~%")
            (uiop:quit 1))
          (progn
            (format t "[check] OK: no direct FiveAM package references in src/~%")
            (uiop:quit 0))))))

(main)
