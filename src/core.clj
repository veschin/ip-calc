(ns core
  (:require [clojure.string :refer [split join]]))

(def config {:mask "255.255.255.0" :ip "192.168.150.0"})

(defn str10->str2
  [string-with-10-base]
  (if (int? string-with-10-base)
    (Integer/toBinaryString string-with-10-base)
    (Integer/toBinaryString (Integer/parseInt string-with-10-base))))

(defn address-to-binary
  [address]
  (map str10->str2 address))

(defn address-to-binary*
  [address]
  (join "." (map str10->str2 address)))

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
  [ip mask]
  (map #(bit-and %1 %2) ip mask))

(defn get-broadcast-address
  [ip mask]
  (let [[ip-part mask-part] (map #(->> (last %)
                                       (Integer/toBinaryString)
                                       (re-seq #"\d")
                                       (map (fn [num] (Integer/parseInt num))))
                                 [ip mask])
        broadcast-part (map #(if (= %1 %2) 1 0) ip-part mask-part)
        broadcast-part-with-10-base (if (and (zero? (last ip)) (zero? (last mask)))
                                      255
                                      (Integer/parseInt (join "" broadcast-part) 2))]
    (conj (pop (vec ip)) broadcast-part-with-10-base)))

(defn get-first-n-last-hosts
  [broadcast-address]
  (let [last-part (last broadcast-address)
        first-n-last (if (= 255 last-part)
                       [1 256]
                       (if (zero? last-part)
                         [last-part (inc last-part)]
                         [(dec last-part) (inc last-part)]))]
    (map #(conj (pop (vec broadcast-address)) %) first-n-last)))

(let [[splitted-mask splitted-ip] (->> (vals config)
                                       (map #(split % #"\."))
                                       (map address-to-nums))
      mask-prefix (get-mask-prefix splitted-mask)
      network-address (get-network-ip splitted-ip splitted-mask)
      wildcard-mask (get-wildcard-mask splitted-mask)
      broadcast-address (get-broadcast-address splitted-ip splitted-mask)
      [min-address max-address] (get-first-n-last-hosts broadcast-address)]

  {:ip [splitted-ip (address-to-binary* splitted-ip)]
   :mask [splitted-mask (address-to-binary* splitted-mask)]
   :mask-prefix mask-prefix
   :network-address [network-address (address-to-binary* network-address)]
   :wildcard-mask [wildcard-mask (address-to-binary* wildcard-mask)]
   :broadcast-address [broadcast-address (address-to-binary* broadcast-address)]
   :min-address [min-address (address-to-binary* min-address)]
   :max-address [max-address (address-to-binary* max-address)]})
