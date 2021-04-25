(defun intercalate (sep &rest nodes)
  "Joins `nodes' into a single `string', putting `sep' between subsequent
elements."
  (let ((acc (car nodes))
        (args (cdr nodes)))
    (if args
        (apply 'make-path (concat acc sep (car args)) (cdr args))
      acc)))

(defun make-path (&rest nodes)
  "Joins `nodes' into a proper file-system path. Path separator is
OS-dependent. The first node is *not* preceded by anything; it's
programmers responsibility to decide whether path should be absolute
or relative."
  (apply 'intercalate (f-path-separator) nodes))

(defun path-list (&rest paths)
  "Joins `paths' into a single string, separating them with a proper
separator (usually ':'). Useful for manipulating variables like PATH."
  (apply 'intercalate path-separator paths))

(defun set-opam-env (switch-dir)
  "Sets environment variables required by `opam' to load modules and
compile projects within Emacs. `switch-dir' is a file-system directory,
where the opam switch to use is located. Roughly equivalent to calling
`eval $(opam env)' in the system shell."
  (let* ((path (getenv "PATH"))
         (manpath (getenv "MANPATH"))
         (vars (pairlis
                '("OPAM_SWITCH_PREFIX"
                  "OCAML_TOPLEVEL_PATH"
                  "PKG_CONFIG_PATH"
                  "PATH"
                  "MANPATH"
                  "CAML_LD_LIBRARY_PATH")
                (list
                 switch-dir
                 (make-path switch-dir "lib" "toplevel")
                 (make-path switch-dir "lib" "pkgconfig")
                 (path-list (make-path switch-dir "bin") path)
                 (path-list (make-path switch-dir "man") manpath)
                 (path-list
                  (make-path switch-dir "lib" "stublibs")
                  (make-path switch-dir "lib" "ocaml" "stublibs")
                  (make-path switch-dir "lib" "ocaml")))
                )))
    (dolist (var vars nil) (setenv (car var) (cdr var)))))
