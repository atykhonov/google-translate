(ert-deftest test-insert-nulls ()
  (should (string-equal
           (google-translate-insert-nulls "[,[,[,,],,],,]")
           "[null,[null,[null,null,null],null,null],null,null]")))
