(ns user.uri.alpha.patch
  (:import
   java.io.UnsupportedEncodingException
   java.net.URLEncoder
   java.net.URLDecoder
   ))


(defn encode-uri-component
  ([s]
   (encode-uri-component s "UTF-8"))
  ([^String s ^String charset]
   (try
     (.. (URLEncoder/encode s charset)
         (replaceAll "\\+" "%20")
         (replaceAll "\\%21" "!")
         (replaceAll "\\%27" "'")
         (replaceAll "\\%28" "(")
         (replaceAll "\\%29" ")")
         (replaceAll "\\%26" "&")
         (replaceAll "\\%3D" "=")
         (replaceAll "\\%7E" "~"))
     (catch UnsupportedEncodingException _
       s))))


(defn decode-uri-component
  ([s]
   (decode-uri-component s "UTF-8"))
  ([^String s ^String charset]
   (URLDecoder/decode s charset)))
