(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.chars :as chars]
    [clojure.string :as str])
  (:import [java.lang Character]))

(def alphabet-lower (vec chars/lowercase))
(def alphabet-len (count alphabet-lower))
(def char->index (zipmap alphabet-lower (range)))

(s/defn encipher-char :- Character
  "Implements a Ceasar cipher with shift N (N=0 => noop)"
  [shift :- s/Int
   char-in :- Character]
  (if-not (chars/alpha? char-in)
    char-in
    (let [idx-plain  (char->index (chars/->lowercase char-in))
          idx-cipher (mod (+ idx-plain shift) alphabet-len)
          char-out   (cond-it-> (nth alphabet-lower idx-cipher)
                       (chars/uppercase? char-in) (chars/->uppercase it))]
      char-out)))

(s/defn encipher :- s/Str
  "Implements a Ceasar cipher with shift N (N=0 => noop)"
  [shift :- s/Int
   msg :- s/Str]
  (let [chars-plain  (str->chars msg)
        chars-cipher (forv [char-plain chars-plain]
                       (encipher-char shift char-plain))
        msg-out      (str/join chars-cipher)]
    msg-out))

(s/defn decipher :- s/Str
  "Deciphers a Ceasar cipher with shift N (N=0 => noop)"
  [shift :- s/Int
   msg :- s/Str]
  (encipher (- shift) msg))

(dotest
  (let [vv (vec (range 6))]
    ; like Python indexing:  https://cljdoc.org/d/tupelo/tupelo/0.9.193/api/tupelo.core#idx
    (is= (idx vv 1) 1)
    (is= (idx vv -1) 5)))

(dotest
  (is= (encipher-char 0 \a) \a)
  (is= (encipher-char 1 \a) \b)
  (is= (encipher-char -1 \a) \z)

  (is= (encipher-char 0 \A) \A)
  (is= (encipher-char 1 \A) \B)
  (is= (encipher-char -1 \A) \Z)
  )

(dotest
  (is= (encipher 1 "Hello") "Ifmmp")
  (is= (encipher 0 "Hello") "Hello")
  (is= (encipher 1 "Hello") "Ifmmp")
  (is= "Hello" (it-> "Hello"
                 (encipher 1 it)
                 (encipher -1 it)))
  (is= "Hello" (it-> "Hello"
                 (encipher 1 it)
                 (decipher 1 it)))

  (let [msg-plain       "James Bond will attack SPECTRE at dawn!"
        cipher-expected "Wnzrf Obaq jvyy nggnpx FCRPGER ng qnja!"
        msg-cipher      (encipher 13 msg-plain)]
    (is= msg-cipher cipher-expected)
    (is= msg-plain (it-> msg-plain
                     (encipher 17 it)
                     (decipher 17 it)))))


