(ns core
  (:require [clojure.string :refer [split join]]))

(def config {:mask "255.255.255.255" :ip "82.179.21.45"})

;; TODO
;; 1) IP адрес в десятичной и в двоичной форме +
;; 2) Префикс маски сети +
;; 3) Маска сети в десятичной и двоичной форме +
;; 4) Обратная маска подсети в десятичной и двоичной форме +
;; 5) IP адрес сети в десятичной и двоичной форме +
;; 6) Широковещательный адрес в десятичной и двоичной форме
;; 7) IP адрес первого хоста в десятичной и двоичной форме
;; 8) IP адрес последнего хоста в десятичной и двоичной форме
;; 9) Количество доступных адресов
;; 10) Количество рабочих адресов для хостов

(defn get-mask-prefix
  [splitted-binary-mask]
  (->> splitted-binary-mask
       (join "")
       (re-seq #"\d")
       frequencies
       vals
       first))

(defn str10->str2
  [string-with-10-base]
  (-> string-with-10-base
      Integer/parseInt
      Integer/toBinaryString))

(defn get-wildcard-mask
  [splitted-mask]
  (let [splitted-wildcard-mask (->> splitted-mask
                                    (map #(Integer/parseInt %))
                                    (map #(bit-xor % 255)))
        wildcard-mask (join "." splitted-wildcard-mask)
        binary-wildcard-mask (join "." (map #(Integer/toBinaryString %) splitted-wildcard-mask))]
    [wildcard-mask binary-wildcard-mask]))

(defn get-network-ip
  "[vec vec] -> [str str]"
  [splitted-ip splitted-mask]
  (let [[ip mask] (partition-all 4 (for [splitted [splitted-ip splitted-mask]
                                         string-with-10-base splitted] (Integer/parseInt string-with-10-base)))
        bit-and-to-binary #(Integer/toBinaryString (bit-and %1 %2))
        splited-address (map #(bit-and %1 %2) ip mask)
        splited-binary-address (map bit-and-to-binary ip mask)]
    (map #(join "." %) [splited-address splited-binary-address])))

(defn get-broadcast-adress [network-address])

(let [{:keys [ip mask]} config
      [splitted-ip splitted-mask] (map #(split % #"\.") [ip mask])
      [splitted-binary-ip splitted-binary-mask] (partition-all 4 (for [splitted [splitted-ip splitted-mask]
                                                                       string-with-10-base splitted]
                                                                   (str10->str2 string-with-10-base)))
      mask-prefix (get-mask-prefix splitted-binary-mask)
      [network-address network-binary-address] (get-network-ip splitted-ip splitted-mask)
      [wildcard-mask binary-wildcard-mask] (get-wildcard-mask splitted-mask)
      broadcast-adress ()]
  {:ip ip :binary-ip (join "." splitted-binary-ip)
   :prefix (str "/" mask-prefix)
   :mask mask :binary-mask (join "." splitted-binary-mask)
   :wildcard-mask wildcard-mask :binary-wildcard-mask binary-wildcard-mask
   :network-address network-address :network-binary-address network-binary-address})

