(ns montoux.codec.core)


(defmacro utf8-header
  "Returns a UTF-8 header byte for a given code point (length prefix plus data bits)."
  [size char]
  (case size
    1 char
    2 `(bit-or 2r11000000 (bit-shift-right ~char 6))
    3 `(bit-or 2r11100000 (bit-shift-right ~char 12))
    4 `(bit-or 2r11110000 (bit-shift-right ~char 18))
    5 `(bit-or 2r11111000 (bit-shift-right ~char 24))
    6 `(bit-or 2r11111100 (bit-shift-right ~char 30))
    ))


(defmacro utf8-continuation
  "Returns a UTF-8 continuation byte for the given code point data (2 bit header plus 6 bits of data)."
  ([char]
   `(utf8-continuation 1 ~char))
  ([n char]
   {:pre [(pos? n)]}
   (let [offset (* 6 (dec n))]
     (if (zero? offset)
       `(bit-or 2r10000000 (bit-and 2r00111111 ~char))
       `(bit-or 2r10000000 (bit-and 2r00111111 (bit-shift-right ~char ~offset)))
       ))))


(defmacro base64-decode-byte
  "Decodes an 8-bit byte from 6-bit base64 bytes. n is one of 0-2 indicating position in 24-bit group."
  [n b0 b1 b2 b3]
  (case n
    0 `(bit-or (bit-and 2r11111100 (bit-shift-left ~b0 2))  ;; 00xxxxxx => xxxxxx00
               (bit-and 2r00000011 (bit-shift-right ~b1 4))) ;; 00xx0000 => 000000xx
    1 `(bit-or (bit-and 2r11110000 (bit-shift-left ~b1 4))  ;; 0000xxxx => xxxx0000
               (bit-and 2r00001111 (bit-shift-right ~b2 2))) ;; 00xxxx00 => 0000xxxx
    2 `(bit-or (bit-and 2r11000000 (bit-shift-left ~b2 6))  ;; 000000xx => xx000000
               (bit-and 2r00111111 ~b3))                    ;; 00xxxxxx => 00xxxxxx
    ))


(defmacro base64-encode-byte
  "Encodes a 6-bit byte from three 8-bit base64 bytes. n is one of 0-3 indicating position in 24-bit group."
  [n b0 b1 b2]
  (case n
    0 `(bit-and 2r00111111 (bit-shift-right ~b0 2))         ;; xxxxxx00 => 00xxxxxx
    1 `(bit-or (bit-and 2r00110000 (bit-shift-left ~b0 4))  ;; 000000xx => 00xx0000
               (bit-and 2r00001111 (bit-shift-right ~b1 4))) ;; xxxx0000 => 0000xxxx
    2 `(bit-or (bit-and 2r00111100 (bit-shift-left ~b1 2))  ;; 0000xxxx => 00xxxx00
               (bit-and 2r00000011 (bit-shift-right ~b2 6))) ;; xx000000 => 000000xx
    3 `(bit-and 2r00111111 ~b2)                             ;; 00xxxxxx => 00xxxxxx
    ))
