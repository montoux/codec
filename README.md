# Montoux Codec

A ClojureScript library for performing stream conversations using a transducer-like API, with full UTF-8/UTF-16 support.

This library fully supports the UTF-8 specification including 3 & 4 byte code points. It includes exhaustive tests
based on [Markus Kuhn's test suite](https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt).

## Usage

Clojure and ClojureScript native strings are UTF-16, but most web content is served as UTF-8. This library provides
tools for converting between different Unicode code point encodings including UTF-8 bytes, UTF-16 native strings,
Base16/Base64, and EDN. 

```clojure
;; UTF-16 to UTF-8 in base64
(codec/encode-string "J̲o̲s̲é̲" codec/base64) ;; => "Ssyyb8yyc8yyw6nMsg=="
(codec/decode-string "Ssyyb8yyc8yyw6nMsg==" codec/base64) ;; => "J̲o̲s̲é̲"

;; ClojureScript data to base64/UTF-8/EDN
(codec/encode-edn {:greeting "Tēnā koutou katoa"} codec/base64) ;; => "ezpncmVldGluZyAiVMSTbsSBIGtvdXRvdSBrYXRvYSJ9"
(codec/decode-edn "ezpncmVldGluZyAiVMSTbsSBIGtvdXRvdSBrYXRvYSJ9" codec/base64) ;; => {:greeting "Tēnā koutou katoa"}

;; Build your own transcoder pipeline using transducer-like composition
(defn encode-string
  "Encode a string to UTF-8 bytes using the given codec."
  [in codec]
  (into-string 
    (comp (utf16->unicode)
          (unicode->utf8)
          (encoder codec))
    in))
```

## Montoux

[Montoux](http://montoux.com) is the global leader in pricing transformation for the life insurance industry.
Our customers include several of the world's leading insurance providers and we are expanding our business in
the United States and Asia, as well as Australia and New Zealand.

## License

Copyright © 2018 [Montoux Limited](https://montoux.com)

The use and distribution terms for this software are covered by the Eclipse Public License 1.0.
By using this software in any fashion, you are agreeing to be bound by the terms of this license.
You must not remove this notice, or any other, from this software.
