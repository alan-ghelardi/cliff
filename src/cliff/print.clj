(ns cliff.print
  (:refer-clojure :exclude [println])
  (:require [clojure.string :as string]))

(def ^:private whitespace " ")

(defn- append-word [^StringBuilder lines word padding-left line-break?]
  (.append lines word)
  (if-not line-break?
    (.append lines whitespace)
    (do (.append lines (System/lineSeparator))
        (dorun (repeatedly padding-left #(.append lines whitespace)))
        lines)))

(defn- line-wrap
  ([text line-width]
   (line-wrap text line-width 0))
  ([text line-width padding-left]
   (letfn [(split-at-whitespaces [text]
             (string/split text #"\s+"))]
     (loop [words-seq            (split-at-whitespaces text)
            current-width        0
            ^StringBuilder lines (StringBuilder.)]
       (let [word       (first words-seq)
             word-width (count word)]
         (cond
           (not word)                                   (.toString lines)
           (>= (+ current-width word-width) line-width) (recur (next words-seq) 0 (append-word lines word padding-left true))
           :else                                        (recur (next words-seq) (+ current-width word-width 1) (append-word lines word padding-left false))))))))

(defn term-width []
  80)

(defn- println
  "Same as clojure.core/println, but wraps the text according to the
  terminal's width."
  [text]
  (clojure.core/println (line-wrap text (term-width))))

(defn- padding-right [text column-width]
  (- column-width (count text)))

(defn- column-width [cells]
  (->> cells (apply max-key count) count))

(defn- left-align [cells]
  (let [column-width (column-width cells)]
    (map #(apply str
                 (cons % (repeat (padding-right % column-width) whitespace)))
         cells)))

(defn left-justify [table]
  (let [transpose #(apply (partial map list) %)]
    (->> table
         transpose
         (map left-align)
         transpose)))

(defn join-cells [cells]
  (if (= 1 (count cells))
    (string/join whitespace cells)
    (let [last-cell       (last cells)
          other-cells     (string/join whitespace (butlast cells))
          padding-left    (inc (count other-cells))
          remaining-width (- (term-width) padding-left)]
      (str other-cells whitespace (line-wrap  last-cell remaining-width padding-left)))))

(defn print-table [table]
  (->> table
       left-justify
       (map join-cells)
       (run! clojure.core/println)))
