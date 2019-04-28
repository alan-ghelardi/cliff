(ns cliff.print)

(defn- line-wrap [words line-width]
  (letfn [(append-word [^StringBuilder lines word line-break?]
            (-> lines
                (.append word)
                (.append " ")
                (cond-> line-break? (.append (System/lineSeparator)))))]
    (loop [words                words
           current-width        0
           ^StringBuilder lines (StringBuilder.)]
      (let [word       (first words)
            word-width (count word)]
        (cond
          (not word)                                   (.toString lines)
          (>= (+ current-width word-width) line-width) (recur (next words) 0 (append-word lines word true))
          :else                                        (recur (next words) (+ current-width word-width 1) (append-word lines word false)))))))
