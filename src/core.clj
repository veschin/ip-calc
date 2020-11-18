(ns core
  (:require [clojure.string :refer [split join]]))

(def config {:mask "255.255.255.12" :ip "82.179.21.45"})

;; TODO
;; 1) IP адрес в десятичной и в двоичной форме +
;; 2) Префикс маски сети +
;; 3) Маска сети в десятичной и двоичной форме +
;; 4) Обратная маска подсети в десятичной и двоичной форме +
;; 5) IP адрес сети в десятичной и двоичной форме +
;; 6) Широковещательный адрес в десятичной и двоичной форме +
;; 7) IP адрес первого хоста в десятичной и двоичной форме
;; 8) IP адрес последнего хоста в десятичной и двоичной форме
;; 9) Количество доступных адресов
;; 10) Количество рабочих адресов для хостов
(defn str10->str2
  [string-with-10-base]
  (if (int? string-with-10-base)
    (Integer/toBinaryString string-with-10-base)
    (Integer/toBinaryString (Integer/parseInt string-with-10-base))))

(defn address-to-binary
  [address]
  (map str10->str2 address))

(defn address-to-nums
  [address]
  (map #(Integer/parseInt %) address))

(defn get-mask-prefix
  [mask]
  (->> mask
       address-to-binary
       (join "")
       (re-seq #"\d")
       frequencies
       vals
       first))

(defn get-wildcard-mask
  [splitted-mask]
  (let [splitted-wildcard-mask (->> splitted-mask
                                    (map #(if (int? %) % (Integer/parseInt %)))
                                    (map #(bit-xor % 255)))]
    splitted-wildcard-mask))

(defn get-network-ip
  "[vec vec] -> [str str]"
  [ip mask]
  (let [splitted-address (map #(bit-and %1 %2) ip mask)]
    splitted-address))

(defn get-broadcast-address
  [address]
  (if (zero? (last address)) (conj (pop (vec address)) 255) address))

(defn get-first-n-last-hosts
  [broadcast-address])

(let [[splitted-mask splitted-ip] (->> (vals config) (map #(split % #"\.")) (map address-to-nums))
      mask-prefix (get-mask-prefix splitted-mask)
      network-address (get-network-ip splitted-ip splitted-mask)
      wildcard-mask (get-wildcard-mask splitted-mask)
      broadcast-address (get-broadcast-address network-address)]
  broadcast-address)
