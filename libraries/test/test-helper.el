(require 'f)
(require 's)
(require 'dash)
(require 'cl-lib)
(require 'ert-expectations)
(require 'el-mock)
(eval-when-compile
  (require 'cl))

(cl-defmacro with-test-rootdir (rootdir &rest body)
  `(progn
     (f-mkdir ,rootdir)
     (unwind-protect
         (progn ,@body)
       (f-delete ,rootdir t))))

