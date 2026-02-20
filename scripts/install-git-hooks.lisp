;;;; install-git-hooks.lisp
;;;;
;;;; Install repository git hooks by writing a forwarding pre-push hook.

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

(defun main ()
  (let* ((root (%project-root))
         (hooks-dir (merge-pathnames ".git/hooks/" root))
         (target (merge-pathnames ".githooks/pre-push" root))
         (hook (merge-pathnames "pre-push" hooks-dir)))
    (unless (probe-file target)
      (format *error-output* "[hooks] Target hook not found: ~a~%" target)
      (uiop:quit 1))
    (ensure-directories-exist hook)
    (with-open-file (out hook
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "#!/usr/bin/env bash~%")
      (format out "set -euo pipefail~%")
      (format out "ROOT_DIR=\"$(git rev-parse --show-toplevel)\"~%")
      (format out "exec \"$ROOT_DIR/.githooks/pre-push\"~%"))
    (uiop:run-program (list "chmod" "+x" (uiop:native-namestring hook))
                      :ignore-error-status nil)
    (format t "[hooks] Installed pre-push hook: ~a~%" (uiop:native-namestring hook))
    (uiop:quit 0)))

(main)
