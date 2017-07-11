(ns card.core
  (:gen-class))


(require '[buddy.core.crypto :as crypto])
(require '[buddy.core.codecs :as codecs])
(require '[buddy.core.nonce :as nonce])
(require '[buddy.core.mac :as mac])
(require '[clojure.string :as str])

(def message (-> (mac/hash "Test@test;Ilike123" {:key "mysecretkey" :alg :hmac+sha256})
        (codecs/bytes->hex)
        (str "Mail;Password;Test@test;Ilike123")))

(def iv8   (nonce/random-nonce 8))
(def key32 (nonce/random-nonce 32))

(def encrypt (let [eng   (crypto/stream-cipher :chacha)
      data  (codecs/to-bytes message)]
  (crypto/init! eng {:key key32 :iv iv8 :op :encrypt})
  (crypto/process-bytes! eng data)))

(if (let [decry  (-> (let [eng   (crypto/stream-cipher :chacha)]
                      (crypto/init! eng {:key key32 :iv iv8 :op :decrypt})
                      (crypto/process-bytes! eng encrypt))
                      (codecs/bytes->str))
              mess    (str/split decry #"Mail;Password;")] 
           (def credentials (-> (second mess)
                                (str/split #";")))
           (mac/verify (second mess)(codecs/hex->bytes (first mess))
             {:key "mysecretkey" :alg :hmac+sha256})) (println "The authentication was successful!") (println "ERROR: The authenication is not valid"))




