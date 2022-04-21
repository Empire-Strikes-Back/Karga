(ns Karga.ipfs-http
  (:require
   [clojure.core.async :as Little-Rock
    :refer [chan put! take! close! offer! to-chan! timeout thread
            sliding-buffer dropping-buffer
            go >! <! alt! alts! do-alts
            mult tap untap pub sub unsub mix unmix admix
            pipe pipeline pipeline-async]]
   [clojure.java.io :as Wichita.java.io]
   [clojure.string :as Wichita.string]
   [clj-http.client]
   [cheshire.core]

   [Karga.seed])
  (:import
   (io.ipfs.api IPFS)
   (java.util.stream Stream)
   (java.util Base64)
   (java.nio.charset StandardCharsets)))

(defn encode-base64url-u
  [^String string]
  (-> (Base64/getUrlEncoder) (.withoutPadding)
      (.encodeToString (.getBytes string StandardCharsets/UTF_8)) (->> (str "u"))))

(defn decode-base64url-u
  [^String string]
  (-> (Base64/getUrlDecoder)
      (.decode (subs string 1))
      (String. StandardCharsets/UTF_8)))

(defn pubsub-sub
  [base-url topic]
  (let [out| (chan (sliding-buffer 10))]
    (thread
      (let [messages (->
                      (clj-http.client/post
                       (str base-url "/api/v0/pubsub/sub")
                       {:query-params {:arg topic}
                        :as :reader}
                       #_(fn [response]
                           (println (type (:body response)))
                           #_(with-open [reader (:body response)]
                               (let [])))
                       #_(fn [ex] (println "exception message is: " (.getMessage ex))))
                      :body
                      (cheshire.core/parsed-seq true))]
        (doseq [message messages]
          (put! out| message))))
    out|))

(defn pubsub-pub
  [base-url topic message]
  (let [out| (chan (sliding-buffer 10))
        response (->
                  (clj-http.client/post
                   (str base-url "/api/v0/pubsub/pub")
                   {:query-params {:arg topic}
                    :async? true
                    :multipart [#_{:name "title" :content "message"}
                                #_{:name "Content/Type" :content "text/plain"}
                                {:name "file" :content message}]}
                   (fn [response]
                     #_(println (= (:status response) 200))
                     #_(put! out| (cheshire.core/parse-string (:body response) true)))
                   (fn [ex] (println "exception message is: " (.getMessage ex)))))]
    out|))