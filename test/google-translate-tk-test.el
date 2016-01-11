(ert-deftest test-google-translate--gen-rl ()
  (should (equal
           (google-translate--gen-rl 403419 "+-a^+6") 415696276.0)))

(ert-deftest test-google-translate--gen-tk ()
  (let ((text "This buffer is for notes you don't want to save, and for Lisp evaluation."))
    (should
     (equal (google-translate--gen-tk text 1627424318) "759658.881332"))
    (should
     (equal (google-translate--gen-tk text 1907276848) "106131.625428"))
    (should
     (equal (google-translate--gen-tk text 1900349679) "536698.15992"))
    (should
     (equal (google-translate--gen-tk text 1691683705) "889864.704400"))
    (should
     (equal (google-translate--gen-tk text 1671310694) "684671.878339"))
    (should
     (equal (google-translate--gen-tk text 1461784924) "448784.60211"))
    (should
     (equal (google-translate--gen-tk text 1974249291) "360864.909202"))
    (should
     (equal (google-translate--gen-tk text 1939139469) "143145.661296"))
    (should
     (equal (google-translate--gen-tk text 1679558583) "253172.311940"))
    (should
     (equal (google-translate--gen-tk text 1912948790) "184141.709859"))))
