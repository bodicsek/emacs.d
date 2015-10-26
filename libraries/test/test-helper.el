(require 'f)
(require 's)
(require 'dash)
(require 'cl-lib)

(cl-defmacro with-test-rootdir (rootdir &rest body)
  `(progn
     (f-mkdir ,rootdir)
     (unwind-protect
         (progn ,@body)
       (f-delete ,rootdir t))))

