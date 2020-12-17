(ns calc
  (:require [clojure.string :refer [join split]]))

(try
  (let [bit-inv-table #(case % 0 1 1 0 :error)
        prefix #(try (reduce + %) (catch Exception _ {:error :wrong-num}))
        fill-zeros #(join "" (repeat % 0))
        extend #(->> % count (- 8) fill-zeros ((fn [zeros] (str zeros %))))
        ->int #(Integer/parseInt %)
        ->bin #(Integer/toBinaryString %)
        ->bin-num #(->> % (map ->bin) (map extend) (join "."))
        bin->int #(Integer/parseInt % 2)
        split* #(try (->> (split %  #"\.") (map ->int)) (catch Exception _ {:error :wrong-input}))
        invert #(->> % ->bin extend (re-seq  #"\d") (map ->int) (map bit-inv-table) (join ""))
        update-last #(->> %1 last %2 (conj (pop (vec %1))))
        view (fn [v] [v (->bin-num v)])

        ip (split* "10.12.1.2")
        mask (split* "240.0.0.0")
        prefix* (prefix (map (comp count #(re-seq #"1" %) extend ->bin) mask))
        wildcart (->> mask (map invert) (map bin->int))
        hosts (->> prefix* (- 32) (Math/pow 2) (#(- % 2)))
        network (map bit-and ip mask)
        broadscast (map bit-or network wildcart)
        hostmin (update-last network inc)
        hostmax (update-last broadscast dec)]
    {:ip (view ip)
     :mask (view mask)
     :prefix prefix*
     :wildcart (view wildcart)
     :hosts hosts
     :network (view network)
     :broadcast (view broadscast)
     :hostmin (view hostmin)
     :hostmax (view hostmax)})
  (catch Exception _ {:error :something-wrong}))
