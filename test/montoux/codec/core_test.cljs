(ns montoux.codec.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [montoux.codec.core :as codec]))


(defn print-hex-number
  "Convert an arbitrary precision number of base16. Caller should provide appropriate context, e.g. 0x..."
  ([n]
   (if (zero? n)
     "0"
     (let [out (array)]
       (print-hex-number out n)
       (.join out ""))))
  ([out n]
   (when-not (zero? n)
     (print-hex-number out (bit-shift-right n 4))
     (.push out (codec/bits->hex (bit-and 0xf n))))))


(defn codec-spy
  "A spy utility for printing intermediate state in a transducer stack.
  Prints intermediate state when transducer terminates."
  [prefix]
  (fn [xf]
    (let [out (array)]
      (fn
        ([] (xf))
        ([result]
         (let [result (xf result)]
           (println prefix (.join out " "))
           result))
        ([result value]
         (let [result (xf result value)]
           (.push out value)
           result))
        ))))


(defn- utf8-decode-raw
  [& stream]
  (binding [codec/unicode-sanitise (fn [size char] char)]
    (into [] (codec/utf8->unicode) stream)))


(defn utf8-decode
  [& bytes]
  (codec/into-string
    (comp (codec/utf8->unicode)
          (codec/unicode->utf16))
    bytes))


;; =====================================================================================================================
;; UTF-8 decoding test cases based on Markus Kuhn's UTF-8 decoder capability and stress test sample
;; =====================================================================================================================


;; https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
;; Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2015-08-28 - CC BY 4.0


(deftest test-utf8-decoding-1-correct-text
  ;; 1. Some correct UTF-8 text

  (is (= "\u03ba\u1f79\u03c3\u03bc\u03b5"
         (utf8-decode 0xce 0xba 0xe1 0xbd 0xb9 0xcf 0x83 0xce 0xbc 0xce 0xb5))
      "Greek word 'kosme'")
  )


(deftest test-utf8-decoding-2-boundary-conditions
  ;; 2. Boundary condition test cases

  (testing "2.1 First possible sequence of a certain length"

    (is (= [0x22 0x0 0x22]
           (utf8-decode-raw 0x22 0x0 0x22))
        "2.1.1 1 byte (U-00000000)")
    (is (= "\"\0\"" (utf8-decode 0x22 0 0x22))
        "2.1.1 1 byte (U-00000000)")

    (is (= [0x22 0x80 0x22]
           (utf8-decode-raw 0x22 0xc2 0x80 0x22))
        "2.1.2 2 bytes (U-00000080)")
    (is (= "\"\u0080\"" (utf8-decode 0x22 0xc2 0x80 0x22))
        "2.1.2 2 bytes (U-00000080)")

    (is (= [0x22 0x800 0x22]
           (utf8-decode-raw 0x22 0xe0 0xa0 0x80 0x22))
        "2.1.3 3 bytes (U-00000800)")
    (is (= "\"\u0800\"" (utf8-decode 0x22 0xe0 0xa0 0x80 0x22))
        "2.1.3 3 bytes (U-00000800)")

    (is (= [0x22 0x10000 0x22]
           (utf8-decode-raw 0x22 0xf0 0x90 0x80 0x80 0x22))
        "2.1.4 4 bytes (U-00010000)")
    (is (= "\"\ud800\udc00\""
           (utf8-decode 0x22 0xf0 0x90 0x80 0x80 0x22))
        "2.1.4 4 bytes (U-00010000)")

    ;; theoretical unicode code points (outside the standard range of 0-10fffff)

    (is (= [0x22 0x200000 0x22]
           (utf8-decode-raw 0x22 0xf8 0x88 0x80 0x80 0x80 0x22))
        "2.1.5 5 bytes (U-00200000)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xf8 0x88 0x80 0x80 0x80 0x22))
        "2.1.5 5 bytes (U-00200000)")

    (is (= [0x22 0x4000000 0x22]
           (utf8-decode-raw 0x22 0xfc 0x84 0x80 0x80 0x80 0x80 0x22))
        "2.1.6 6 bytes (U-04000000)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xfc 0x84 0x80 0x80 0x80 0x80 0x22))
        "2.1.6 6 bytes (U-04000000)")

    )

  (testing "2.2  Last possible sequence of a certain length"

    (is (= [0x22 0x7f 0x22]
           (utf8-decode-raw 0x22 0x7f 0x22))
        "2.2.1  1 byte  (U-0000007F)")
    (is (= "\"\u007f\""
           (utf8-decode 0x22 0x7f 0x22))
        "2.2.1  1 byte  (U-0000007F)")

    (is (= [0x22 0x7ff 0x22]
           (utf8-decode-raw 0x22 0xdf 0xbf 0x22))
        "2.2.2  2 bytes (U-000007FF)")
    (is (= "\"\u07ff\""
           (utf8-decode 0x22 0xdf 0xbf 0x22))
        "2.2.2  2 bytes (U-000007FF)")

    (is (= [0x22 0xffff 0x22]
           (utf8-decode-raw 0x22 0xef 0xbf 0xbf 0x22))
        "2.2.3  3 bytes (U-0000FFFF)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xef 0xbf 0xbf 0x22))
        "2.2.3  3 bytes (U-0000FFFF), illegal code point")

    ;; theoretical unicode code points (outside the standard range of 0-10fffff)

    (is (= [0x22 0x1fffff 0x22]
           (utf8-decode-raw 0x22 0xf7 0xbf 0xbf 0xbf 0x22))
        "2.2.4  4 bytes (U-001FFFFF)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xf7 0xbf 0xbf 0xbf 0x22))
        "2.2.4  4 bytes (U-001FFFFF), outside the legal range")

    (is (= [0x22 0x3ffffff 0x22]
           (utf8-decode-raw 0x22 0xfb 0xbf 0xbf 0xbf 0xbf 0x22))
        "2.2.5  5 bytes (U-03FFFFFF)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xfb 0xbf 0xbf 0xbf 0xbf 0x22))
        "2.2.5  5 bytes (U-03FFFFFF), outside the legal range")

    (is (= [0x22 0x7fffffff 0x22]
           (utf8-decode-raw 0x22 0xfd 0xbf 0xbf 0xbf 0xbf 0xbf 0x22))
        "2.2.6  6 bytes (U-7FFFFFFF)")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xfd 0xbf 0xbf 0xbf 0xbf 0xbf 0x22))
        "2.2.6  6 bytes (U-7FFFFFFF), outside the legal range")

    )

  (testing "2.3 Other boundary conditions"
    (is (= [0x22 0xd7ff 0x22]
           (utf8-decode-raw 0x22 0xed 0x9f 0xbf 0x22))
        "2.3.1 U-0000D7FF = ed 9f bf")
    (is (= "\"\ud7ff\""
           (utf8-decode 0x22 0xed 0x9f 0xbf 0x22))
        "2.3.1 U-0000D7FF = ed 9f bf")

    (is (= [0x22 0xe000 0x22]
           (utf8-decode-raw 0x22 0xee 0x80 0x80 0x22))
        "2.3.2 U-0000E000 = ee 80 80")
    (is (= "\"\ue000\""
           (utf8-decode 0x22 0xee 0x80 0x80 0x22))
        "2.3.2 U-0000E000 = ee 80 80")

    (is (= [0x22 0xfffd 0x22]
           (utf8-decode-raw 0x22 0xef 0xbf 0xbd 0x22))
        "2.3.3 U-0000FFFD = ef bf bd")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xef 0xbf 0xbd 0x22))
        "2.3.3 U-0000FFFD = ef bf bd")

    (is (= [0x22 0x10ffff 0x22]
           (utf8-decode-raw 0x22 0xf4 0x8f 0xbf 0xbf 0x22))
        "2.3.4 U-0010FFFF = f4 8f bf bf")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xf4 0x8f 0xbf 0xbf 0x22))
        "2.3.4 U-0010FFFF = f4 8f bf bf, illegal code point")

    (is (= [0x22 0x110000 0x22]
           (utf8-decode-raw 0x22 0xf4 0x90 0x80 0x80 0x22))
        "2.3.5 U-00110000 = f4 90 80 80")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xf4 0x90 0x80 0x80 0x22))
        "2.3.5 U-00110000 = f4 90 80 80")
    ))


(deftest test-utf8-decoding-3-malformed-sequences
  ;; 3  Malformed sequences

  (testing "3.1  Unexpected continuation bytes"

    ;; Each unexpected continuation byte should be separately signalled as a malformed sequence of its own.

    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0x80 0x22))
        "3.1.1  First continuation byte 0x80")
    (is (= "\"\ufffd\""
           (utf8-decode 0x22 0xbf 0x22))
        "3.1.2  Last  continuation byte 0xbf")
    (is (= "\"\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x22))
        "3.1.3  2 continuation bytes")
    (is (= "\"\ufffd\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x80 0x22))
        "3.1.4  3 continuation bytes")
    (is (= "\"\ufffd\ufffd\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x80 0xbf 0x22))
        "3.1.5  4 continuation bytes")
    (is (= "\"\ufffd\ufffd\ufffd\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x80 0xbf 0x80 0x22))
        "3.1.6  5 continuation bytes")
    (is (= "\"\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x80 0xbf 0x80 0xbf 0x22))
        "3.1.7  6 continuation bytes")
    (is (= "\"\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\ufffd\""
           (utf8-decode 0x22 0x80 0xbf 0x80 0xbf 0x80 0xbf 0x80 0x22))
        "3.1.8  7 continuation bytes")
    (is (= (str "����������������"
                "����������������"
                "����������������"
                "����������������")
           (apply utf8-decode (range 0x80 0xc0)))
        "3.1.9  Sequence of all 64 possible continuation bytes (0x80-0xbf)")

    )

  (testing "3.2  Lonely start characters"

    (is (= (apply str (repeat 32 "\ufffd "))
           (apply utf8-decode (interleave (range 0xc0 0xe0) (repeat 0x20))))
        "3.2.1 All 32 first bytes of 2-byte sequences (0xc0-0xdf), each followed by a space character")

    (is (= (apply str (repeat 16 "\ufffd "))
           (apply utf8-decode (interleave (range 0xe0 0xf0) (repeat 0x20))))
        "3.2.2 All 16 first bytes of 3-byte sequences (0xe0-0xef), each followed by a space character")

    (is (= (apply str (repeat 8 "\ufffd "))
           (apply utf8-decode (interleave (range 0xf0 0xf8) (repeat 0x20))))
        "3.2.3 All 8 first bytes of 4-byte sequences (0xf0-0xf7), each followed by a space character")

    (is (= (apply str (repeat 4 "\ufffd "))
           (apply utf8-decode (interleave (range 0xf8 0xfc) (repeat 0x20))))
        "3.2.4 All 4 first bytes of 5-byte sequences (0xf8-0xfb), each followed by a space character")

    (is (= (apply str (repeat 2 "\ufffd "))
           (apply utf8-decode (interleave (range 0xfc 0xfe) (repeat 0x20))))
        "3.2.5 All 2 first bytes of 6-byte sequences (0xfc-0xfd), each followed by a space character")

    )

  (testing "3.3  Sequences with last continuation byte missing"

    ;; All bytes of an incomplete sequence should be signalled as a single
    ;; malformed sequence, i.e., you should see only a single replacement
    ;; character in each of the next 10 tests. (Characters as in section 2)

    (is (= "\"�\""
           (utf8-decode 0x22 0xc0 0x22))
        "3.3.1  2-byte sequence with last byte missing (U+0000)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xe0 0x80 0x22))
        "3.3.2  3-byte sequence with last byte missing (U+0000)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xf0 0x80 0x80 0x22))
        "3.3.3  4-byte sequence with last byte missing (U+0000)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xf8 0x80 0x80 0x80 0x22))
        "3.3.4  5-byte sequence with last byte missing (U+0000)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xfc 0x80 0x80 0x80 0x80 0x22))
        "3.3.5  6-byte sequence with last byte missing (U+0000)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xdf 0x22))
        "3.3.6  2-byte sequence with last byte missing (U-000007FF)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xef 0xbf 0x22))
        "3.3.7  3-byte sequence with last byte missing (U-0000FFFF)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xf7 0xbf 0xbf 0x22))
        "3.3.8  4-byte sequence with last byte missing (U-001FFFFF)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xfb 0xbf 0xbf 0xbf 0x22))
        "3.3.9  5-byte sequence with last byte missing (U-03FFFFFF)")
    (is (= "\"�\""
           (utf8-decode 0x22 0xfd 0xbf 0xbf 0xbf 0xbf 0x22))
        "3.3.10 6-byte sequence with last byte missing (U-7FFFFFFF)")

    )

  (testing "3.4  Concatenation of incomplete sequences"

    (is (= "����������"
           (utf8-decode 0xc0
                        0xe0 0x80
                        0xf0 0x80 0x80
                        0xf8 0x80 0x80 0x80
                        0xfc 0x80 0x80 0x80 0x80
                        0xdf
                        0xef 0xbf
                        0xf7 0xbf 0xbf
                        0xfb 0xbf 0xbf 0xbf
                        0xfd 0xbf 0xbf 0xbf 0xbf))
        "All the 10 sequences of 3.3 concatenated, you should see 10 malformed sequences being signalled")

    )

  (testing "3.5  Impossible bytes"

    ;; The following two bytes cannot appear in a correct UTF-8 string

    (is (= "\"�\""
           (utf8-decode 0x22 0xfe 0x22))
        "3.5.1  fe")
    (is (= "\"�\""
           (utf8-decode 0x22 0xff 0x22))
        "3.5.2  ff")
    (is (= "\"����\""
           (utf8-decode 0x22 0xfe 0xfe 0xff 0xff 0x22))
        "3.5.3  fe fe ff ff")

    ))


(deftest test-utf8-decoding-4-overlong-sequences

  ;; 4 Overlong sequences

  ;; The following sequences are not malformed according to the letter of
  ;; the Unicode 2.0 standard. However, they are longer then necessary and
  ;; a correct UTF-8 encoder is not allowed to produce them. A "safe UTF-8
  ;; decoder" should reject them just like malformed sequences for two
  ;; reasons: (1) It helps to debug applications if overlong sequences are
  ;; not treated as valid representations of characters, because this helps
  ;; to spot problems more quickly. (2) Overlong sequences provide
  ;; alternative representations of characters, that could maliciously be
  ;; used to bypass filters that check only for ASCII characters. For
  ;; instance, a 2-byte encoded line feed (LF) would not be caught by a
  ;; line counter that counts only 0x0a bytes, but it would still be
  ;; processed as a line feed by an unsafe UTF-8 decoder later in the
  ;; pipeline. From a security point of view, ASCII compatibility of UTF-8
  ;; sequences means also, that ASCII characters are *only* allowed to be
  ;; represented by ASCII bytes in the range 0x00-0x7f. To ensure this
  ;; aspect of ASCII compatibility, use only "safe UTF-8 decoders" that
  ;; reject overlong UTF-8 sequences for which a shorter encoding exists.

  ;; 4.1 Examples of an overlong ASCII character

  ;; With a safe UTF-8 decoder, all of the following five overlong
  ;; representations of the ASCII character slash ("/") should be rejected
  ;; like a malformed UTF-8 sequence, for instance by substituting it with
  ;; a replacement character. If you see a slash below, you do not have a
  ;; safe UTF-8 decoder!

  (is (= "\"�\""
         (utf8-decode 0x22 0xc0 0xaf 0x22))
      "4.1.1 U+002F = 0xc0 0xaf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xe0 0x80 0xaf 0x22))
      "4.1.2 U+002F = 0xe0 0x80 0xaf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf0 0x80 0x80 0xaf 0x22))
      "4.1.3 U+002F = 0xf0 0x80 0x80 0xaf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf8 0x80 0x80 0x80 0xaf 0x22))
      "4.1.4 U+002F = 0xf8 0x80 0x80 0x80 0xaf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xfc 0x80 0x80 0x80 0x80 0xaf 0x22))
      "4.1.5 U+002F = 0xfc 0x80 0x80 0x80 0x80 0xaf")

  ;; 4.2 Maximum overlong sequences

  ;; Below you see the highest Unicode value that is still resulting in an
  ;; overlong sequence if represented with the given number of bytes. This
  ;; is a boundary test for safe UTF-8 decoders. All five characters should
  ;; be rejected like malformed UTF-8 sequences.

  (is (= "\"�\""
         (utf8-decode 0x22 0xc1 0xbf 0x22))
      "4.2.1 U-0000007F = 0xc1 0xbf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xe0 0x9f 0xbf 0x22))
      "4.2.2 U-000007FF = 0xe0 0x9f 0xbf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf0 0x8f 0xbf 0xbf 0x22))
      "4.2.3 U-0000FFFF = 0xf0 0x8f 0xbf 0xbf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf8 0x87 0xbf 0xbf 0xbf 0x22))
      "4.2.4 U-001FFFFF = 0xf8 0x87 0xbf 0xbf 0xbf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xfc 0x83 0xbf 0xbf 0xbf 0xbf 0x22))
      "4.2.5 U-03FFFFFF = 0xfc 0x83 0xbf 0xbf 0xbf 0xbf")

  ;; 4.3 Overlong representation of the NUL character

  ;; The following five sequences should also be rejected like malformed
  ;; UTF-8 sequences and should not be treated like the ASCII NUL
  ;; character.

  (is (= "\"�\""
         (utf8-decode 0x22 0xc0 0x80 0x22))
      "4.3.1 U+0000 = 0xc0 0x80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xe0 0x80 0x80 0x22))
      "4.3.2 U+0000 = 0xe0 0x80 0x80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf0 0x80 0x80 0x80 0x22))
      "4.3.3 U+0000 = 0xf0 0x80 0x80 0x80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xf8 0x80 0x80 0x80 0x80 0x22))
      "4.3.4 U+0000 = 0xf8 0x80 0x80 0x80 0x80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xfc 0x80 0x80 0x80 0x80 0x80 0x22))
      "4.3.5 U+0000 = 0xfc 0x80 0x80 0x80 0x80 0x80")
  )


(deftest test-utf8-decoding-5-illegal-code-positions

  ;; 5 Illegal code positions

  ;; The following UTF-8 sequences should be rejected like malformed
  ;; sequences, because they never represent valid ISO 10646 characters and
  ;; a UTF-8 decoder that accepts them might introduce security problems
  ;; comparable to overlong UTF-8 sequences.

  ;; 5.1 Single UTF-16 surrogates

  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xa0 0x80 0x22))
      "5.1.1 U+D800 = ed a0 80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xad 0xbf 0x22))
      "5.1.2 U+DB7F = ed ad bf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xae 0x80 0x22))
      "5.1.3 U+DB80 = ed ae 80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xaf 0xbf 0x22))
      "5.1.4 U+DBFF = ed af bf")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xb0 0x80 0x22))
      "5.1.5 U+DC00 = ed b0 80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xbe 0x80 0x22))
      "5.1.6 U+DF80 = ed be 80")
  (is (= "\"�\""
         (utf8-decode 0x22 0xed 0xbf 0xbf 0x22))
      "5.1.7 U+DFFF = ed bf bf")

  ;; 5.2 Paired UTF-16 surrogates

  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xa0 0x80 0xed 0xb0 0x80 0x22))
      "5.2.1 U+D800 U+DC00 = ed a0 80 ed b0 80")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xa0 0x80 0xed 0xbf 0xbf 0x22))
      "5.2.2 U+D800 U+DFFF = ed a0 80 ed bf bf")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xad 0xbf 0xed 0xb0 0x80 0x22))
      "5.2.3 U+DB7F U+DC00 = ed ad bf ed b0 80")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xad 0xbf 0xed 0xbf 0xbf 0x22))
      "5.2.4 U+DB7F U+DFFF = ed ad bf ed bf bf")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xae 0x80 0xed 0xb0 0x80 0x22))
      "5.2.5 U+DB80 U+DC00 = ed ae 80 ed b0 80")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xae 0x80 0xed 0xbf 0xbf 0x22))
      "5.2.6 U+DB80 U+DFFF = ed ae 80 ed bf bf")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xaf 0xbf 0xed 0xb0 0x80 0x22))
      "5.2.7 U+DBFF U+DC00 = ed af bf ed b0 80")
  (is (= "\"��\""
         (utf8-decode 0x22 0xed 0xaf 0xbf 0xed 0xbf 0xbf 0x22))
      "5.2.8 U+DBFF U+DFFF = ed af bf ed bf bf")

  ;; 5.3 Noncharacter code positions

  ;; The following "noncharacters" are "reserved for internal use" by
  ;; applications, and according to older versions of the Unicode Standard
  ;; "should never be interchanged" . Unicode Corrigendum #9 dropped the
  ;; latter restriction. Nevertheless, their presence in incoming UTF-8 data
  ;; can remain a potential security risk, depending on what use is made of
  ;; these codes subsequently. Examples of such internal use:
  ;;
  ;; - Some file APIs with 16-bit characters may use the integer value -1
  ;; = U+FFFF to signal an end-of-file (EOF) or error condition.
  ;;
  ;; - In some UTF-16 receivers, code point U+FFFE might trigger a
  ;; byte-swap operation (to convert between UTF-16LE and UTF-16BE) .
  ;;
  ;; With such internal use of noncharacters, it may be desirable and safer
  ;; to block those code points in UTF-8 decoders, as they should never
  ;; occur legitimately in incoming UTF-8 data, and could trigger unsafe
  ;; behaviour in subsequent processing.
  ;;
  ;; Particularly problematic noncharacters in 16-bit applications:

  (is (= "\"�\""
         (utf8-decode 0x22 0xef 0xbf 0xbe 0x22))
      "5.3.1 U+FFFE = ef bf be")
  (is (= "\"�\""
         (utf8-decode 0x22 0xef 0xbf 0xbf 0x22))
      "5.3.2 U+FFFF = ef bf bf")

  ;; Other noncharacters:

  (is (= "��������������������������������"
         (apply utf8-decode (range 0xfdd0 0xfdf0)))
      "5.3.3 U+FDD0 .. U+FDEF")

  (is (= "\"��������������������������������\""
         (utf8-decode
           0x22
           0xf0 0x9f 0xbf 0xbe 0xf0 0x9f 0xbf 0xbf
           0xf0 0xaf 0xbf 0xbe 0xf0 0xaf 0xbf 0xbf
           0xf0 0xbf 0xbf 0xbe 0xf0 0xbf 0xbf 0xbf
           0xf1 0x8f 0xbf 0xbe 0xf1 0x8f 0xbf 0xbf
           0xf1 0x9f 0xbf 0xbe 0xf1 0x9f 0xbf 0xbf
           0xf1 0xaf 0xbf 0xbe 0xf1 0xaf 0xbf 0xbf
           0xf1 0xbf 0xbf 0xbe 0xf1 0xbf 0xbf 0xbf
           0xf2 0x8f 0xbf 0xbe 0xf2 0x8f 0xbf 0xbf
           0xf2 0x9f 0xbf 0xbe 0xf2 0x9f 0xbf 0xbf
           0xf2 0xaf 0xbf 0xbe 0xf2 0xaf 0xbf 0xbf
           0xf2 0xbf 0xbf 0xbe 0xf2 0xbf 0xbf 0xbf
           0xf3 0x8f 0xbf 0xbe 0xf3 0x8f 0xbf 0xbf
           0xf3 0x9f 0xbf 0xbe 0xf3 0x9f 0xbf 0xbf
           0xf3 0xaf 0xbf 0xbe 0xf3 0xaf 0xbf 0xbf
           0xf3 0xbf 0xbf 0xbe 0xf3 0xbf 0xbf 0xbf
           0xf4 0x8f 0xbf 0xbe 0xf4 0x8f 0xbf 0xbf
           0x22))
      "5.3.4 U+nFFFE U+nFFFF (for n = 1..16)")
  )


;; =====================================================================================================================
;; UTF-8 encoding test cases inspired by Markus Kuhn's UTF-8 decoder capability and stress test sample
;; =====================================================================================================================


(defn utf8-encode
  "Encode a UTF-16 string or unicode code points to a UTF-8 byte array."
  [& input]
  (if (and (= 1 (count input)) (string? (first input)))
    (vec
      (codec/into-bytes
        (comp (codec/utf16->unicode)
              (codec/unicode->utf8))
        (first input)))
    (vec
      (codec/into-bytes
        (codec/unicode->utf8)
        input))))


(defn utf8-encode-raw
  "Encode to UTF-8, bypassing 'legal codepoint' checks to test full range of encoder."
  [& input]
  (binding [codec/unicode-sanitise identity]
    (apply utf8-encode input)))


(def invalid-utf8
  [0x22 0xef 0xbf 0xbd 0x22])


(deftest test-utf8-encoding-1-correct-text
  ;; 1. Some correct UTF-8 text

  (is (= [0xce 0xba 0xe1 0xbd 0xb9 0xcf 0x83 0xce 0xbc 0xce 0xb5]
         (utf8-encode "κόσμε"))
      "Greek word 'kosme'")
  )


(deftest test-utf8-encoding-2-boundary-conditions
  ;; 2. Boundary condition test cases

  (testing "2.1 First possible sequence of a certain length"

    (is (= [0x22 0 0x22]
           (utf8-encode 0x22 0 0x22))
        "2.1.1 1 byte (U-00000000)")

    (is (= [0x22 0xc2 0x80 0x22]
           (utf8-encode 0x22 0x80 0x22))
        "2.1.2 2 bytes (U-00000080)")

    (is (= [0x22 0xe0 0xa0 0x80 0x22]
           (utf8-encode 0x22 0x800 0x22))
        "2.1.3 3 bytes (U-00000800)")

    (is (= [0x22 0xf0 0x90 0x80 0x80 0x22]
           (utf8-encode 0x22 0x10000 0x22))
        "2.1.4 4 bytes (U-00010000)")

    ;; theoretical unicode code points (outside the standard range of 0-10fffff)

    (is (= [0x22 0xf8 0x88 0x80 0x80 0x80 0x22]
           (utf8-encode-raw 0x22 0x200000 0x22))
        "2.1.5 5 bytes (U-00200000)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x200000 0x22))
        "2.1.5 5 bytes (U-00200000)")

    (is (= [0x22 0xfc 0x84 0x80 0x80 0x80 0x80 0x22]
           (utf8-encode-raw 0x22 0x4000000 0x22))
        "2.1.6 6 bytes (U-04000000)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x4000000 0x22))
        "2.1.6 6 bytes (U-04000000)")
    )

  (testing "2.2  Last possible sequence of a certain length"

    (is (= [0x22 0x7f 0x22]
           (utf8-encode 0x22 0x7f 0x22))
        "2.2.1  1 byte  (U-0000007F)")

    (is (= [0x22 0xdf 0xbf 0x22]
           (utf8-encode 0x22 0x7ff 0x22))
        "2.2.2  2 bytes (U-000007FF)")

    (is (= [0x22 0xef 0xbf 0xbf 0x22]
           (utf8-encode-raw 0x22 0xffff 0x22))
        "2.2.3  3 bytes (U-0000FFFF)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0xffff 0x22))
        "2.2.3  3 bytes (U-0000FFFF) (non-character)")

    ;; theoretical unicode code points (outside the standard range of 0-10fffff)

    (is (= [0x22 0xf7 0xbf 0xbf 0xbf 0x22]
           (utf8-encode-raw 0x22 0x1fffff 0x22))
        "2.2.4  4 bytes (U-001FFFFF)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x1fffff 0x22))
        "2.2.4  4 bytes (U-001FFFFF)")

    (is (= [0x22 0xfb 0xbf 0xbf 0xbf 0xbf 0x22]
           (utf8-encode-raw 0x22 0x3ffffff 0x22))
        "2.2.5  5 bytes (U-03FFFFFF)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x3ffffff 0x22))
        "2.2.5  5 bytes (U-03FFFFFF)")

    (is (= [0x22 0xfd 0xbf 0xbf 0xbf 0xbf 0xbf 0x22]
           (utf8-encode-raw 0x22 0x7fffffff 0x22))
        "2.2.6  6 bytes (U-7FFFFFFF)")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x7fffffff 0x22))
        "2.2.6  6 bytes (U-7FFFFFFF)")
    )

  (testing "2.3 Other boundary conditions"
    (is (= [0x22 0xed 0x9f 0xbf 0x22]
           (utf8-encode 0x22 0xd7ff 0x22))
        "2.3.1 U-0000D7FF = ed 9f bf")

    (is (= [0x22 0xee 0x80 0x80 0x22]
           (utf8-encode 0x22 0xe000 0x22))
        "2.3.2 U-0000E000 = ee 80 80")

    (is (= invalid-utf8
           (utf8-encode 0x22 0xfffd 0x22))
        "2.3.3 U-0000FFFD = ef bf bd")

    (is (= [0x22 0xf4 0x8f 0xbf 0xbf 0x22]
           (utf8-encode-raw 0x22 0x10ffff 0x22))
        "2.3.4 U-0010FFFF = f4 8f bf bf")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x10ffff 0x22))
        "2.3.4 U-0010FFFF = f4 8f bf bf")

    (is (= [0x22 0xf4 0x90 0x80 0x80 0x22]
           (utf8-encode-raw 0x22 0x110000 0x22))
        "2.3.5 U-00110000 = f4 90 80 80")
    (is (= invalid-utf8
           (utf8-encode 0x22 0x110000 0x22))
        "2.3.5 U-00110000 = f4 90 80 80")
    ))


(deftest test-utf8-encoding-3-illegal-code-positions

  ;; The following UTF-8 sequences should be rejected like malformed
  ;; sequences, because they never represent valid ISO 10646 characters and
  ;; a UTF-8 decoder that accepts them might introduce security problems
  ;; comparable to overlong UTF-8 sequences.

  ;; 3.1 Single UTF-16 surrogates

  (is (= invalid-utf8
         (utf8-encode 0x22 0xd800 0x22))
      "3.1.1 U+D800 = ed a0 80")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdb7f 0x22))
      "3.1.2 U+DB7F = ed ad bf")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdb80 0x22))
      "3.1.3 U+DB80 = ed ae 80")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdbff 0x22))
      "3.1.4 U+DBFF = ed af bf")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdc00 0x22))
      "3.1.5 U+DC00 = ed b0 80")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdf80 0x22))
      "3.1.6 U+DF80 = ed be 80")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xdfff 0x22))
      "3.1.7 U+DFFF = ed bf bf")

  ;; 3.2 Paired UTF-16 surrogates

  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xd800 0xdc00 0x22))
      "3.2.1 U+D800 U+DC00 = ed a0 80 ed b0 80")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xd800 0xdfff 0x22))
      "3.2.2 U+D800 U+DFFF = ed a0 80 ed bf bf")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdb7f 0xdc00 0x22))
      "3.2.3 U+DB7F U+DC00 = ed ad bf ed b0 80")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdb7f 0xdfff 0x22))
      "3.2.4 U+DB7F U+DFFF = ed ad bf ed bf bf")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdb80 0xdc00 0x22))
      "3.2.5 U+DB80 U+DC00 = ed ae 80 ed b0 80")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdb80 0xdfff 0x22))
      "3.2.6 U+DB80 U+DFFF = ed ae 80 ed bf bf")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdbff 0xdc00 0x22))
      "3.2.7 U+DBFF U+DC00 = ed af bf ed b0 80")
  (is (= [0x22 0xef 0xbf 0xbd 0xef 0xbf 0xbd 0x22]
         (utf8-encode 0x22 0xdbff 0xdfff 0x22))
      "3.2.8 U+DBFF U+DFFF = ed af bf ed bf bf")

  ;; 3.3 Noncharacter code positions

  ;; The following "noncharacters" are "reserved for internal use" by
  ;; applications, and according to older versions of the Unicode Standard
  ;; "should never be interchanged" . Unicode Corrigendum #9 dropped the
  ;; latter restriction. Nevertheless, their presence in incoming UTF-8 data
  ;; can remain a potential security risk, depending on what use is made of
  ;; these codes subsequently. Examples of such internal use:
  ;;
  ;; - Some file APIs with 16-bit characters may use the integer value -1
  ;; = U+FFFF to signal an end-of-file (EOF) or error condition.
  ;;
  ;; - In some UTF-16 receivers, code point U+FFFE might trigger a
  ;; byte-swap operation (to convert between UTF-16LE and UTF-16BE) .
  ;;
  ;; With such internal use of noncharacters, it may be desirable and safer
  ;; to block those code points in UTF-8 decoders, as they should never
  ;; occur legitimately in incoming UTF-8 data, and could trigger unsafe
  ;; behaviour in subsequent processing.
  ;;
  ;; Particularly problematic noncharacters in 16-bit applications:

  (is (= invalid-utf8
         (utf8-encode 0x22 0xfffe 0x22))
      "3.3.1 U+FFFE = ef bf be")
  (is (= invalid-utf8
         (utf8-encode 0x22 0xffff 0x22))
      "3.3.2 U+FFFF = ef bf bf")

  ;; Other noncharacters:

  (is (= (flatten
           [0x22
            (repeat 32 [0xef 0xbf 0xbd])
            0x22])
         (apply utf8-encode (concat [0x22] (range 0xfdd0 0xfdf0) [0x22])))
      "3.3.3 U+FDD0 .. U+FDEF")

  (is (= (flatten
           [0x22
            (repeat 32 [0xef 0xbf 0xbd])
            0x22])
         (apply utf8-encode (concat [0x22]
                                    (for [x (range 0x10000 0x110000 0x10000)] (bit-or x 0xfffe))
                                    (for [x (range 0x10000 0x110000 0x10000)] (bit-or x 0xffff))
                                    [0x22])))
      "3.3.4 U+nFFFE U+nFFFF (for n = 1..16)")

  (is (= invalid-utf8
         (utf8-encode 0x22 -1 0x22))
      "3.3.5  negative numbers are not code points")

  (is (= invalid-utf8
         (utf8-encode-raw 0x22 0x80000000 0x22))
      "3.3.6  U-80000000: outside the range of code points that can be represented by UTF-8")
  )


;; =====================================================================================================================
;; UTF-16 decoding test cases
;; =====================================================================================================================


(defn utf16-decode [input]
  (vec
    (codec/into-bytes
      (codec/utf16->unicode)
      input)))


(defn utf16-decode-raw [& input]
  (binding [codec/unicode-sanitise identity
            codec/to-char-code     identity]
    (utf16-decode input)))


(deftest test-utf16-decoding-1-correct-text
  ;; 1. Some correct UTF-16 text

  (is (= [0x03ba 0x1f79 0x03c3 0x03bc 0x03b5]
         (utf16-decode "\u03ba\u1f79\u03c3\u03bc\u03b5"))
      "Greek word 'kosme'")
  )


(deftest test-utf16-decoding-2-boundary-conditions
  ;; 2. Boundary condition test cases

  (testing "2.1 single code point boundaries"

    (is (= [0x22 0 0x22]
           (utf16-decode "\"\0\""))
        "2.1.1 U-00000000")

    (is (= [0x22 0xd7ff 0x22]
           (utf16-decode "\"\ud7ff\""))
        "2.1.2 U-0000d7ff")

    (is (= [0x22 0xe000 0x22]
           (utf16-decode "\"\ue000\""))
        "2.1.3 U-0000e000")

    (is (= [0x22 0xffff 0x22]
           (utf16-decode-raw 0x22 0xffff 0x22))
        "2.1.4 U-0000ffff")
    )

  (testing "2.2 lone surrogate boundaries"

    (is (= [0x22 0xd800 0x22]
           (utf16-decode-raw 0x22 0xd800 0x22))
        "2.2.3 U-0000d800")

    (is (= [0x22 0xdbff 0x22]
           (utf16-decode-raw 0x22 0xdbff 0x22))
        "2.2.4 U-0000dbff")

    (is (= [0x22 0xdc00 0x22]
           (utf16-decode-raw 0x22 0xdc00 0x22))
        "2.2.5 U-0000dc00")

    (is (= [0x22 0xdfff 0x22]
           (utf16-decode-raw 0x22 0xdfff 0x22))
        "2.2.6 U-0000dfff")
    )

  (testing "2.3 surrogate pair boundaries"

    (is (= [0x22 0x10000 0x22]
           (utf16-decode "\"\ud800\udc00\""))
        "2.3.1 U-00010000 (surrogate pair lower bound)")

    (is (= [0x22 0x103ff 0x22]
           (utf16-decode "\"\ud800\udfff\""))
        "2.3.2 U-00013fff (lower surrogate upper bound)")

    (is (= [0x22 0x10400 0x22]
           (utf16-decode "\"\ud801\udc00\""))
        "2.3.3 U-00010400 (lower surrogate overflow)")

    (is (= [0x22 0x10ffff 0x22]
           (utf16-decode-raw 0x22 0xdbff 0xdfff 0x22))
        "2.3.2 U-0010ffff (invalid character, surrogate pair upper bound)")
    )
  )


;; =====================================================================================================================
;; UTF-16 encoding test cases
;; =====================================================================================================================


(defn utf16-encode [& input]
  (vec
    (codec/into-bytes
      (comp (codec/unicode->utf16)
            (mapcat seq)
            (map codec/to-char-code))
      input)))


(defn utf16-encode-raw [& input]
  (binding [codec/unicode-sanitise identity
            codec/from-char-code   identity]
    (vec (codec/into-bytes
           (codec/unicode->utf16)
           input))))


(deftest test-utf16-encoding-1-correct-text
  ;; 1. Some correct UTF-16 text

  (is (= [0x03ba 0x1f79 0x03c3 0x03bc 0x03b5]
         (utf16-encode 0x03ba 0x1f79 0x03c3 0x03bc 0x03b5))
      "Greek word 'kosme'")
  )


(deftest test-utf16-encoding-2-boundary-conditions
  ;; 2. Boundary condition test cases

  (testing "2.1 single code point boundaries"

    (is (= [0x22 0 0x22]
           (utf16-encode 0x22 0 0x22))
        "2.1.1 U-00000000")

    (is (= [0x22 0xd7ff 0x22]
           (utf16-encode 0x22 0xd7ff 0x22))
        "2.1.2 U-0000d7ff")

    (is (= [0x22 0xe000 0x22]
           (utf16-encode 0x22 0xe000 0x22))
        "2.1.3 U-0000e000")

    (is (= [0x22 0xffff 0x22]
           (utf16-encode-raw 0x22 0xffff 0x22))
        "2.1.4 U-0000ffff (not a character, single character upper limit)")
    )

  (testing "2.2 lone surrogate boundaries"

    ;; lone surrogates are generally allowed

    (is (= [0x22 0xd800 0x22]
           (utf16-encode-raw 0x22 0xd800 0x22))
        "2.2.3 U-0000d800")

    (is (= [0x22 0xdbff 0x22]
           (utf16-encode-raw 0x22 0xdbff 0x22))
        "2.2.4 U-0000dbff")

    (is (= [0x22 0xdc00 0x22]
           (utf16-encode-raw 0x22 0xdc00 0x22))
        "2.2.5 U-0000dc00")

    (is (= [0x22 0xdfff 0x22]
           (utf16-encode-raw 0x22 0xdfff 0x22))
        "2.2.6 U-0000dfff")
    )

  (testing "2.3 surrogate pair boundaries"

    (is (= [0x22 0xd800 0xdc00 0x22]
           (utf16-encode 0x22 0x10000 0x22))
        "2.3.1 U-00010000")

    (is (= [0x22 0xd800 0xdfff 0x22]
           (utf16-encode 0x22 0x103ff 0x22))
        "2.3.2 U-00013fff")

    (is (= [0x22 0xd801 0xdc00 0x22]
           (utf16-encode 0x22 0x10400 0x22))
        "2.3.3 U-00010400")

    (is (= [0x22 0xdbff 0xdfff 0x22]
           (utf16-encode-raw 0x22 0x10ffff 0x22))
        "2.3.2 U-0010ffff (not a character, UTF-16 upper limit)")
    )

  (testing "2.4 invalid codes"

    (is (= [0x22 0xfffd 0x22]
           (utf16-encode 0x22 -1 0x22))
        "2.4.1 -1")

    (is (= [0x22 0xfffd 0x22]
           (utf16-encode 0x22 0x110000 0x22))
        "2.4.2 U-00110000")
    )
  )


;; =====================================================================================================================
;; Codec test cases
;; =====================================================================================================================


(deftest test-encode-data
  (testing "base64"
    (is (= ""
           (codec/encode-data (into-array []) codec/base64))
        "empty data array")
    (is (= "AAA+AAA/"
           (codec/encode-data (into-array [0 0 62 0 0 63]) codec/base64))
        "non-url characters")
    )
  (testing "base64url"
    (is (= ""
           (codec/encode-data (into-array []) codec/base64url))
        "empty data array")
    (is (= "AAA-AAA_"
           (codec/encode-data (into-array [0 0 62 0 0 63]) codec/base64url))
        "url-safe characters")
    )
  )


(deftest test-encode-string
  (testing "base64"
    (is (= ""
           (codec/encode-string "" codec/base64))
        "empty")
    (is (= "Zm9vbw=="
           (codec/encode-string "fooo" codec/base64))
        "string that requires padding")
    (is (= "Ssyyb8yyc8yyw6nMsg=="
           (codec/encode-string "J̲o̲s̲é̲" codec/base64))
        "utf-8")
    (is (= "VGVzdCBVc2VyIPCfmII="
           (codec/encode-string "Test User \uD83D\uDE02" codec/base64))
        "utf-8 4-bytes")
    )
  (testing "base64url"
    (is (= ""
           (codec/encode-string "" codec/base64url))
        "empty")
    (is (= "Zm9vbw"
           (codec/encode-string "fooo" codec/base64url))
        "string that requires padding -- no padding should be produced")
    (is (= "Ssyyb8yyc8yyw6nMsg"
           (codec/encode-string "J̲o̲s̲é̲" codec/base64url))
        "utf-8")
    (is (= "VGVzdCBVc2VyIPCfmII"
           (codec/encode-string "Test User \uD83D\uDE02" codec/base64url))
        "utf-8 4-bytes")
    )
  )


(deftest test-encode-edn
  (testing "base64"
    (is (= "bmls"
           (codec/encode-edn nil codec/base64))
        "nil")
    (is (= "OkrMsm/MsnPMssOpzLI="
           (codec/encode-edn :J̲o̲s̲é̲ codec/base64))
        "utf-8")
    )
  (testing "base64url"
    (is (= "bmls"
           (codec/encode-edn nil codec/base64url))
        "nil")
    (is (= "OkrMsm_MsnPMssOpzLI"
           (codec/encode-edn :J̲o̲s̲é̲ codec/base64url))
        "utf-8")
    )
  )


(deftest test-decode-data
  (testing "base64"
    (is (= []
           (vec (codec/decode-data "" codec/base64)))
        "empty data array")
    (is (= [0 0 62 0 0 63]
           (vec (codec/decode-data "AAA+AAA/" codec/base64)))
        "non-url characters")
    )
  (testing "base64url"
    (is (= []
           (vec (codec/decode-data "" codec/base64url)))
        "empty data array")
    (is (= [0 0 62 0 0 63]
           (vec (codec/decode-data "AAA-AAA_" codec/base64url)))
        "url-safe characters")
    )
  )


(deftest test-decode-string
  (testing "base64"
    (is (= ""
           (codec/decode-string "" codec/base64))
        "empty")
    (is (= "fooo"
           (codec/decode-string "Zm9vbw==" codec/base64))
        "string that requires padding")
    (is (= "J̲o̲s̲é̲"
           (codec/decode-string "Ssyyb8yyc8yyw6nMsg==" codec/base64))
        "utf-8")
    (is (= "Test User \uD83D\uDE02"
           (codec/decode-string "VGVzdCBVc2VyIPCfmII=" codec/base64))
        "utf-8 4-bytes")
    )
  (testing "base64url"
    (is (= ""
           (codec/decode-string "" codec/base64url))
        "empty")
    (is (= "fooo"
           (codec/decode-string "Zm9vbw" codec/base64url))
        "string that requires padding -- no padding should be produced")
    (is (= "J̲o̲s̲é̲"
           (codec/decode-string "Ssyyb8yyc8yyw6nMsg" codec/base64url))
        "utf-8")
    (is (= "Test User \uD83D\uDE02"
           (codec/decode-string "VGVzdCBVc2VyIPCfmII" codec/base64url))
        "utf-8 4-bytes")
    )
  )


(deftest test-decode-edn
  (testing "base64"
    (is (= nil
           (codec/decode-edn "bmls" codec/base64))
        "nil")
    (is (= :J̲o̲s̲é̲
           (codec/decode-edn "OkrMsm/MsnPMssOpzLI=" codec/base64))
        "utf-8")
    )
  (testing "base64url"
    (is (= nil
           (codec/decode-edn "bmls" codec/base64url))
        "nil")
    (is (= :J̲o̲s̲é̲
           (codec/decode-edn "OkrMsm_MsnPMssOpzLI" codec/base64url))
        "utf-8")
    )
  )
