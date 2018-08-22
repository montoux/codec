(ns montoux.codec.core
  (:require-macros [montoux.codec.core :refer [base64-decode-byte base64-encode-byte utf8-header utf8-continuation]])
  (:require [cljs.reader :refer [read-string]])
  (:import (goog.string StringBuffer)))


(defn into-string
  "Returns a string constructed from applying the given transducing stack to the input sequence."
  [xf inputs]
  (let [f (fn
            ([] (StringBuffer.))
            ([sb] (.toString sb))
            ([sb c] (.append sb c) sb)
            )]
    (transduce xf f inputs)))


(defn into-bytes
  "Returns an array constructed from applying the given transducing stack to the input sequence."
  [xf inputs]
  (let [f (fn
            ([] (array))
            ([out] out)
            ([out token] (.push out token) out)
            )]
    (transduce xf f inputs)))


(def ^:private bits->hex
  {0x0 "0" 0x1 "1" 0x2 "2" 0x3 "3" 0x4 "4" 0x5 "5" 0x6 "6" 0x7 "7"
   0x8 "8" 0x9 "9" 0xa "a" 0xb "b" 0xc "c" 0xd "d" 0xe "e" 0xf "f"})


(defn bytes->base16
  "Trancode bytes to base16 characters."
  []
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result byte]
       (-> result
           (xf (bits->hex (bit-and 0xf (bit-shift-right byte 4))))
           (xf (bits->hex (bit-and 0xf byte))))
        ))))


(defn base16->bytes
  "Transcode base16 characters to bytes."
  []
  (fn [xf]
    (let [!b (volatile! nil)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result char]
         (let [i (case char
                   "0" 0x0 "1" 0x1 "2" 0x2 "3" 0x3 "4" 0x4 "5" 0x5 "6" 0x6 "7" 0x7 "8" 0x8 "9" 0x9
                   "A" 0xA "B" 0xB "C" 0xC "D" 0xD "E" 0xE "F" 0xF
                   "a" 0xA "b" 0xB "c" 0xC "d" 0xD "e" 0xE "f" 0xF)]
           (if @!b
             (let [high @!b
                   low  i]
               (vreset! !b nil)
               (xf result (bit-or (bit-shift-left high 4) low)))
             (do (vreset! !b i)
                 result))))
        ))))


(def ^:private base64-default-alphabet
  (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       "abcdefghijklmnopqrstuvwxyz"
       "0123456789+/"))


(def ^:private base64-websafe-alphabet
  (str "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       "abcdefghijklmnopqrstuvwxyz"
       "0123456789-_"))


(def ^:private base64-byte->char
  (into {} (map-indexed vector base64-default-alphabet)))


(def ^:private base64-byte->websafe-char
  (into {} (map-indexed vector base64-websafe-alphabet)))


(def ^:private base64-char->byte
  (into {} (map-indexed (fn [a b] [b a]) base64-default-alphabet)))


(def ^:private base64-websafe-char->byte
  (into {} (map-indexed (fn [a b] [b a]) base64-websafe-alphabet)))


(defn- base64-padding? [char]
  (= "=" char))


(defn base64->bytes
  "Transcode base64 characters to bytes. Takes groups of four 6-bit inputs and converts them to three 8-bit outputs."
  [web-safe?]
  (fn [xf]
    (let [alphabet (if web-safe?
                     base64-websafe-char->byte
                     base64-char->byte)
          buffer   (array)
          put      (fn
                     ([result]
                      result)
                     ([result b0]
                       ;; invalid, incomplete data
                      result)
                     ([result b0 b1]
                       ;; 6 bits + 2 bits => 8 bits
                      (xf result (base64-decode-byte 0 b0 b1 0 0)))
                     ([result b0 b1 b2]
                       ;; 6 bits + 2 bits / 4 bits + 4 bits => 8 bits + 8 bits
                      (-> result
                          (xf (base64-decode-byte 0 b0 b1 b2 0))
                          (xf (base64-decode-byte 1 b0 b1 b2 0))))
                     ([result b0 b1 b2 b3]
                       ;; complete 24 bit sequence => 3 bytes
                      (-> result
                          (xf (base64-decode-byte 0 b0 b1 b2 b3))
                          (xf (base64-decode-byte 1 b0 b1 b2 b3))
                          (xf (base64-decode-byte 2 b0 b1 b2 b3))))
                     )
          ]
      (fn
        ([] (xf))
        ([result]
         (xf (apply put result buffer)))
        ([result char]
         (let [b (get alphabet char ::not-found)]
           (if (= ::not-found b)
             (cond (base64-padding? char)
                   result
                   (re-matches #"^\s$" char)
                   result
                   :else
                   (throw (js/Error (str "invalid character in base64: " char))))
             (do (.push buffer b)
                 (if (== 4 (alength buffer))
                   (let [b0 (aget buffer 0)
                         b1 (aget buffer 1)
                         b2 (aget buffer 2)
                         b3 (aget buffer 3)]
                     (set! (.-length buffer) 0)
                     (put result b0 b1 b2 b3))
                   result)))))
        ))))


(defn bytes->base64
  "Transcode from bytes to base64 using the specified alphabet (standard or websafe). Takes groups of three 8-bit inputs
  and converts them to four 6-bit characters."
  [web-safe?]
  (fn [xf]
    (let [alphabet (if web-safe?
                     base64-byte->websafe-char
                     base64-byte->char)
          padding  (if web-safe? "" "=")
          buffer   (array)
          put      (fn
                     ([result]
                       ;; nothing to do
                      result)
                     ([result b0]
                       ;; 8 bits => 6 bits / 2 bits + 4 padding
                      (-> result
                          (xf (get alphabet (base64-encode-byte 0 b0 0 0)))
                          (xf (get alphabet (base64-encode-byte 1 b0 0 0)))
                          (xf padding)
                          (xf padding)
                          ))
                     ([result b0 b1]
                       ;; 8 bits + 8 bits => 6 bits / 2 bits + 4 bits / 4 bits + 2 padding
                      (-> result
                          (xf (get alphabet (base64-encode-byte 0 b0 b1 0)))
                          (xf (get alphabet (base64-encode-byte 1 b0 b1 0)))
                          (xf (get alphabet (base64-encode-byte 2 b0 b1 0)))
                          (xf padding)
                          ))
                     ([result b0 b1 b2]
                       ;; 8 bits + 8 bits + 8 bits => 6 bits / 2 bits + 4 bits / 4 bits + 2 bits / 6 bits
                      (-> result
                          (xf (get alphabet (base64-encode-byte 0 b0 b1 b2)))
                          (xf (get alphabet (base64-encode-byte 1 b0 b1 b2)))
                          (xf (get alphabet (base64-encode-byte 2 b0 b1 b2)))
                          (xf (get alphabet (base64-encode-byte 3 b0 b1 b2)))
                          )))
          ]
      (fn
        ([] (xf))
        ([result]
         (xf (apply put result buffer)))
        ([result byte]
         (.push buffer (bit-and 0xff byte))
         (if (== 3 (alength buffer))
           (let [b0 (aget buffer 0)
                 b1 (aget buffer 1)
                 b2 (aget buffer 2)]
             (set! (.-length buffer) 0)
             (put result b0 b1 b2))
           result))
        ))))


(def
  ^{:doc     "Unicode replacement character, used in place of invalid code points."
    :private true}
  replacement
  0xfffd)


(defn unicode-sanitise
  "Sanitise unicode code points by replacing overlong and non-character code points with the unicode replacement
  character (0xfffd). Optionally takes a UTF-8 codepoint size to check for overlong encodings."
  {:dynamic            true
   :public-for-testing true}
  ([char]
    ;; eliminate non-characters
   (if (or
         ;; codepoints must be positive
         (neg? char)
         ;; 0x10ffff is the max supported codepoint
         (< 0x10ffff char)
         ;; surrogate code points
         (<= 0xd800 char 0xdfff)
         ;; non-characters
         (<= 0xfdd0 char 0xfdef)
         ;; any character ending in fffe or ffff is non-character (utf-16 byte swap and 16-bit -1)
         (== 0xfffe (bit-and char 0xfffe)))
     replacement
     char))
  ([size char]
   (let [char (unicode-sanitise char)]
     ;; eliminate overlong code-points -- these should never be produced by a valid UTF-8 encoder, and although not
     ;; explicitly forbidden by the spec, they are a security risk as they allow multiple encodings of the same char.
     (case size
       ;; invalid size
       0 replacement
       1 (if (<= 0 char 0x7f) char replacement)
       2 (if (<= 0x80 char 0x7ff) char replacement)
       3 (if (<= 0x800 char 0xffff) char replacement)
       ;; unicode doesn't actually allow characters greater than 21 bits at present
       4 (if (<= 0x10000 char 0x10ffff) char replacement)
       replacement
       ))))


(defn unicode->utf8
  "Transcode from unicode code points to UTF-8 bytes."
  []
  (fn [xf]
    (fn unicode->utf8*
      ([] (xf))
      ([result] (xf result))
      ([result char]
       (let [char (unicode-sanitise char)]
         (cond
           (<= char 0x7f)
           (xf result char)

           (<= char 0x7ff)
           (-> result
               (xf (utf8-header 2 char))
               (xf (utf8-continuation 1 char)))

           (<= char 0xffff)
           (-> result
               (xf (utf8-header 3 char))
               (xf (utf8-continuation 2 char))
               (xf (utf8-continuation 1 char)))

           (<= char 0x1fffff)
           (-> result
               (xf (utf8-header 4 char))
               (xf (utf8-continuation 3 char))
               (xf (utf8-continuation 2 char))
               (xf (utf8-continuation 1 char)))

           (<= char 0x3ffffff)
           (-> result
               (xf (utf8-header 5 char))
               (xf (utf8-continuation 4 char))
               (xf (utf8-continuation 3 char))
               (xf (utf8-continuation 2 char))
               (xf (utf8-continuation 1 char)))

           (<= char 0x7fffffff)
           (-> result
               (xf (utf8-header 6 char))
               (xf (utf8-continuation 5 char))
               (xf (utf8-continuation 4 char))
               (xf (utf8-continuation 3 char))
               (xf (utf8-continuation 2 char))
               (xf (utf8-continuation 1 char)))

           :else
           (unicode->utf8* result replacement)
           ))))))


(defn- utf8-continuation?
  "Is the given byte a UTF-8 continuation byte?"
  [byte]
  (== 2r10000000 (bit-and 2r11000000 byte)))


(defn- utf8-leading-byte->code-point-size
  "Given a utf-8 leading byte, return the size of the sequence, in bytes."
  [byte]
  (cond (<= 0xfc byte) 6
        (<= 0xf8 byte) 5
        (<= 0xf0 byte) 4
        (<= 0xe0 byte) 3
        (<= 0xc0 byte) 2
        :else 1))


(defn utf8->unicode
  "Transcode from utf-8 bytes to unicode code points."
  []
  (fn [xf]
    (let [!char (volatile! 0)
          !size (volatile! 0)
          !more (volatile! 0)]
      (fn utf8-bytes->unicode*
        ([] (xf))
        ([result]
         (if (zero? @!more)
           (xf result)
           (xf (xf result replacement))))
        ([result byte]
         (cond
           ;; continuation byte, either we're reading more already or we have an error
           (utf8-continuation? byte)
           (let [char (bit-or (bit-shift-left @!char 6)
                              (bit-and 2r00111111 byte))]
             (case @!more
               ;; error, not expecting continuation
               0 (xf result replacement)
               ;; end of code point, write and continue
               1 (let [char (unicode-sanitise @!size char)]
                   (vreset! !char 0)
                   (vreset! !size 0)
                   (vreset! !more 0)
                   (xf result char))
               ;; more to come
               (do
                 (vreset! !char char)
                 (vswap! !more dec)
                 result)))

           ;; start of a new code point, but expecting more bytes
           (pos? @!more)
           ;; retry reading this code point after pushing an error and discarding continuation
           (do
             (vreset! !char 0)
             (vreset! !size 0)
             (vreset! !more 0)
             (utf8-bytes->unicode* (xf result replacement) byte))

           ;; start of a new code point
           :else
           (let [size (utf8-leading-byte->code-point-size byte)
                 char (case size
                        1 (bit-and 2r01111111 byte)
                        2 (bit-and 2r00011111 byte)
                        3 (bit-and 2r00001111 byte)
                        4 (bit-and 2r00000111 byte)
                        5 (bit-and 2r00000011 byte)
                        6 (bit-and 2r00000011 byte))]
             (if (= 1 size)
               (xf result char)
               (do (vreset! !char char)
                   (vreset! !size size)
                   (vreset! !more (dec size))
                   result)))
           ))
        ))))


(defn- utf16-high-surrogate?
  "A 16-bit number is a high surrogate if it's of the form 0xd800 + 0x03ff"
  [c]
  (== 0xd800 (bit-and 0xfc00 c)))


(defn- utf16-low-surrogate?
  "A 16-bit number is a low-surrogate if it's of the form 0xdc00 + 0x03ff"
  [c]
  (== 0xdc00 (bit-and 0xfc00 c)))


(defn to-char-code
  "Given a javascript UTF-16 string and an index, return a unicode code point."
  {:dynamic            true
   :public-for-testing true}
  ([string]
   (.charCodeAt string 0))
  ([string index]
   (.charCodeAt string index)))


(defn utf16->unicode
  "Transcode from javascript utf-16 characters to unicode code points."
  []
  (fn [xf]
    (let [!c (volatile! nil)]
      (fn utf16->unicode*
        ([] (xf))
        ([result]
         (if-let [c @!c]
           (xf (xf result c))
           (xf result)))
        ([result input]
         (let [char (to-char-code input 0)]
           (cond
             ;; surrogate pair
             (and @!c (utf16-low-surrogate? char))
             (let [c1 @!c
                   c2 char
                   c  (+ 0x10000
                         (bit-shift-left (bit-and c1 0x03ff) 10)
                         (bit-and c2 0x3ff))]
               (vreset! !c nil)
               (xf result (unicode-sanitise c)))

             ;; previously read high surrogate is not a pair member, write and recur
             @!c
             (let [c @!c]
               (vreset! !c nil)
               (utf16->unicode* (xf result (unicode-sanitise c)) input))

             ;; high surrogate, add to buffer
             (utf16-high-surrogate? char)
             (do (vreset! !c char)
                 result)

             ;; literal code point
             :else
             (xf result (unicode-sanitise char)))))
        ))))


(defn from-char-code
  "Given a numerical unicode code point, returns a javascript UTF-16 string."
  {:dynamic            true
   :public-for-testing true}
  [c]
  (.fromCharCode js/String c))


(defn unicode->utf16
  "Transcoder from unicode characters to UTF-16 characters."
  []
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result c]
       (let [c (unicode-sanitise c)]
         (cond
           ;; literal code point
           (<= 0x0 c 0xd7ff)
           (xf result (from-char-code c))
           ;; illegal unicode characters, but most people accept them
           (<= 0xd800 c 0xdfff)
           (xf result (from-char-code c))
           ;; literal code point
           (<= 0xe000 c 0xffff)
           (xf result (from-char-code c))
           ;; surrogate pair
           (<= 0x10000 c 0x10ffff)
           (let [c  (- c 0x10000)
                 c1 (bit-or 0xd800 (bit-and 0x3ff (bit-shift-right c 10)))
                 c2 (bit-or 0xdc00 (bit-and 0x3ff c))]
             (-> result
                 (xf (from-char-code c1))
                 (xf (from-char-code c2))))
           :else
           ;; illegal code point, replace with placeholder
           (xf result (from-char-code replacement))
           ))))))


;; =====================================================================================================================
;; Encoding/Decoding
;; =====================================================================================================================


(def base16 ::base16)
(def base64 ::base64)
(def base64url ::base64url)


(defn encoder
  "Return an encoding transcoder for the given string encoding."
  [codec]
  (case codec
    ::base16 (bytes->base16)
    ::base64 (bytes->base64 false)
    ::base64url (bytes->base64 true)
    ))


(defn decoder
  "Return a decoding transcoder for the given string encoding."
  [codec]
  (case codec
    ::base16 (base16->bytes)
    ::base64 (base64->bytes false)
    ::base64url (base64->bytes true)
    ))


(defn encode-data
  "Encode a byte array to a string using the given codec."
  [bytes codec]
  (into-string (encoder codec) bytes))


(defn encode-string
  "Encode a string to UTF-8 bytes using the given codec."
  [in codec]
  (into-string (comp (utf16->unicode)
                     (unicode->utf8)
                     (encoder codec))
               in))


(defn encode-edn
  "Render the given input as EDN then encode as UTF-8 bytes using the given codec."
  [in codec]
  (-> (pr-str in)
      (encode-string codec)))


(defn decode-data
  "Decode a byte array from a string using the given encoding."
  [data codec]
  (into-bytes (decoder codec) data))


(defn decode-string
  "Decode a string from the given data string using the given codec, assuming UTF-8 bytes."
  [in codec]
  (into-string
    (comp (decoder codec)
          (utf8->unicode)
          (unicode->utf16))
    in))


(defn decode-edn
  "Read EDN data from a UTF-8 string encoded with the given codec."
  [in codec]
  (-> (decode-string in codec)
      (read-string)))
