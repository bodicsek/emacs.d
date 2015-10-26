(load-file "path-setup.el")

(ert-deftest ps--dirs-test/root-is-returned-if-eligible ()
  (with-test-rootdir "test/bin"
                     (let ((eligible-test-dirs (ps--dirs "test/bin" '("bin"))))
                       ;; there should be only one eligible directory
                       (should (equal (-count 's-present? eligible-test-dirs) 1))
                       ;; the eligible directory is the root directory
                       (should (-all?
                                (lambda (dir) (s-ends-with? "test/bin" dir))
                                eligible-test-dirs)))))

(ert-deftest ps--dirs-test/recursive-eligible-dirs ()
  (with-test-rootdir "test/rec"
                     (f-mkdir "test/rec/bin" "tool" "bin")
                     (let ((eligible-test-dirs (ps--dirs "test/rec" '("bin"))))
                       (should (s-ends-with? "test/rec/bin" (-first-item eligible-test-dirs))))))

(ert-deftest ps--dirs-test/multi-eligibility-criteria ()
  (with-test-rootdir "test/root"
                     (f-mkdir "test/root/tool1" "bin")
                     (f-mkdir "test/root/tool2" "lib")
                     (let ((eligible-test-dirs (ps--dirs "test/root" '("bin" "lib"))))
                       (should (s-ends-with? "test/root/tool1/bin" (-first-item eligible-test-dirs)))
                       (should (s-ends-with? "test/root/tool2/lib" (-first-item (-drop 1 eligible-test-dirs)))))))

(ert-deftest ps--map-bin-lib-dirs/lambda-is-called-for-every-bin-and-lib-dir ()
  (with-test-rootdir "test/root"
                     (f-mkdir "test/root/bin" "lib")
                     (let ((collection ()))
                       (ps--map-bin-lib-dirs "test/root" (lambda (d) (push d collection)))
                       (should (equal (length collection) 2))
                       (should (s-ends-with? "test/root/bin/lib" (car collection)))
                       (should (s-ends-with? "test/root/bin" (cadr collection))))))

(ert-deftest ps--exec-path-setup/eligible-dirs-are-added-to-exec-path ()
  (with-test-rootdir "test/root"
                     (f-mkdir "test/root/bin")
                     (f-mkdir "test/root/lib")
                     ;; precondition
                     (should (-none? (lambda (e) (s-contains? "test/root" e)) exec-path))
                     (ps--exec-path-setup "test/root")
                     ;; postcondition
                     (should (equal (-count (lambda (e) (s-contains? "test/root" e)) exec-path) 2))))

(ert-deftest ps--path-setup/eligible-dirs-are-added-to-path ()
  (with-test-rootdir "test/root"
                     (f-mkdir "test/root/bin")
                     (f-mkdir "test/root/lib")
                     ;; precondition
                     (should (-none? (lambda (e) (s-contains? "test/root" e)) (s-split ";" (getenv "PATH"))))
                     (ps--path-setup "test/root")
                     ;; postcondition
                     (should (equal (-count (lambda (e) (s-contains? "test/root" e)) (s-split ";" (getenv "PATH"))) 2))))
