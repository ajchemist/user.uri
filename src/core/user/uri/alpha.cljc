(ns user.uri.alpha
  (:require
   [clojure.string :as str]
   [clojure.walk :refer [keywordize-keys]]
   #?(:clj [user.uri.alpha.patch :as patch])
   ))


(defn- parse-int
  [s]
  #?(:clj (Integer/parseInt s)
     :cljs (js/parseInt s)))


(defn- parse-path
  "Parse a value from a serialized query-string key index. If the
  index value is empty 0 is returned, if it's a digit it returns the
  js/parseInt value, otherwise it returns the extracted index."
  [path]
  (let [index-re #"\[([^\]]*)\]*" ;; Capture the index value.
        parts (re-seq index-re path)]
    (map
     (fn [[_ part]]
       (cond
        (empty? part) 0
        (re-matches #"\d+" part) (parse-int part)
        :else part))
     parts)))


(defn- key-parse
  "Return a key path for a serialized query-string entry.
  Ex.
    (key-parse \"foo[][a][][b]\")
    ;; => (\"foo\" 0 \"a\" 0 \"b\")
  "
  [k]
  (let [re #"([^\[\]]+)((?:\[[^\]]*\])*)?"
        [_ key path] (re-matches re k)
        parsed-path (when path (parse-path path))]
    (cons key parsed-path)))


(defn- assoc-in-query-params
  "Like assoc-in but numbers in path create vectors instead of maps.
  Ex.
    (assoc-in-query-params {} [\"foo\" 0] 1)
    ;; => {\"foo\" [1]}
    (assoc-in-query-params {} [\"foo\" 0 \"a\"] 1)
    ;; => {\"foo\" [{\"a\" 1}]}
  "
  [m path v]
  (let [heads (fn [xs]
                (map-indexed
                  (fn [i _]
                    (take (inc i) xs))
                  xs))
        hs (heads path)
        m (reduce
            (fn [m h]
              (if (and (number? (last h))
                       (not (vector? (get-in m (butlast h)))))
                (assoc-in m (butlast h) [])
                m))
            m
            hs)]
    (if (zero? (last path))
      (update-in m (butlast path) conj v)
      (assoc-in m path v))))


;; * api


(defn encode-uri-component
  [s]
  #?(:clj
     (patch/encode-uri-component s)
     :cljs
     (js/encodeURIComponent s)))


(defn decode-uri-component
  [s]
  #?(:clj
     (patch/decode-uri-component s)
     :cljs
     (js/decodeURIComponent s)))


(defn decode-query-string
  [query-string]
  (let [parts  (str/split query-string #"&")
        params (reduce
                (fn [m part]
                  ;; We only want two parts since the part on the right hand side
                  ;; could potentially contain an =.
                  (let [[k v] (str/split part #"=" 2)]
                    (assoc-in-query-params m (key-parse k) (decode-uri-component v))))
                {}
                parts)
        params (keywordize-keys params)]
    params))


(defn extract-query-params
  [uri]
  (let [[_ query-string] (str/split uri #"\?")]
    (when query-string
      (decode-query-string query-string))))
