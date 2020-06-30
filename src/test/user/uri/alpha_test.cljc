(ns user.uri.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [user.uri.alpha :as user.uri]
   ))


(deftest main
  (is
   (= (-> "http://abc.xyz/?id=100&name=안녕하세요"
        user.uri/encode-uri-component
        user.uri/decode-uri-component)
      "http://abc.xyz/?id=100&name=안녕하세요")))
