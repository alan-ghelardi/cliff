(ns cliff.specs
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(def not-blank-string-gen (gen/such-that (complement string/blank?)
                                         (gen/string)))

(def not-blank-string? (s/with-gen #(and (string? %)
                                         (not (string/blank? %)))
                         (constantly not-blank-string-gen)))

(s/def ::name keyword?)

(s/def ::desc not-blank-string?)

(s/def ::type #{:boolean :int :string})

(def shorthand-gen (gen/fmap (partial str)
                         (gen/char-alphanumeric)))

(s/def ::shorthand (s/with-gen #(and (string? %)
                                     (= 1 (count %)))
                     (constantly shorthand-gen)))

(s/def ::required? boolean)

(s/def ::default any?)

(s/def ::list? boolean?)

(s/def ::values (s/coll-of any? :kind set? :min-count 1))

(s/def ::argument (s/keys :req-un [::name ::type]
                          :opt-un [::desc ::required? ::default ::values]))

(s/def flag (s/keys :req-un [::type]
                    :opt-un [::desc ::required? ::shorthand ::default ::list? ::values]))

(s/def ::arguments (s/coll-of ::argument :kind vector?))

(s/def ::flags (s/map-of ::name ::flag))
