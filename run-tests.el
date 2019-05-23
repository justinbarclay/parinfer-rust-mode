(let* ((project-tests-files '("paren-parinfer-tests.el" "smart-parinfer-tests.el" "indent-parinfer-tests.el"))
       (current-directory (file-name-directory load-file-name))
       (project-test-path (expand-file-name "./test/" current-directory))
       (project-root-path (expand-file-name "." current-directory)))

  ;; add the package being tested to 'load-path so it can be 'require-d
  (add-to-list 'load-path project-root-path)
  (add-to-list 'load-path project-test-path)

  ;; load the files with tests
  (mapcar
   (lambda (project-tests-file)
     (load (expand-file-name project-tests-file project-test-path) t t))
   project-tests-files)

  ;; run the tests
  (ert-run-tests-batch-and-exit 't))
