;; Steps to recreate:
;; 1. place cursor at (10 . 53)
;; 2. run paredit-forward-barf-sexp
;; Expected state:
;; (foo- _foo [foo foo]
;;       (foo/foo foo {:foo foo-foo
;;                     :foo foo-foo
;;                     :foo-foo (foo [foo-foo]
;;                                   (foo/foo-foo! foo :foo-foo foo-foo)
;;                                   (foo-foo foo
;;                                            (foo
;;                                             foo
;;                                             {:foo_foo foo-foo
;;                                              :foo foo})
;;                                            foo-foo))})
;;       (foo2
;;        foo/Foo
;;        (foo-foo [_]
;;                 {:foo-foo "Foo"
;;                  :foo foo
;;                  :foo foo})
;;        foo/Foo
;;        (foo-foo [_]
;;                 (foo (foo-foo foo [:foo-foo :foo])
;;                      (foo/foo "/foo/foo_foo" #(foo/foo-foo! foo :foo-foo %)))))

(foo- _foo [foo foo]
      (foo/foo foo {:foo foo-foo
                    :foo foo-foo
                    :foo-foo (foo [foo-foo]
                                  (foo/foo-foo! foo :foo-foo foo-foo)
                                  (foo-foo foo
                                           (foo
                                            foo
                                            {:foo_foo foo-foo
                                             :foo foo})
                                           foo-foo))}
               (foo2
                foo/Foo
                (foo-foo [_]
                         {:foo-foo "Foo"
                          :foo foo
                          :foo foo})
                foo/Foo
                (foo-foo [_]
                         (foo (foo-foo foo [:foo-foo :foo])
                              (foo/foo "/foo/foo_foo" #(foo/foo-foo! foo :foo-foo %)))))))
