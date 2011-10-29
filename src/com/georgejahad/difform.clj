;;; difform.clj - diffs two clojure forms

;; Copyright (c) 2010 George Jahad All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be found
;; in the file epl-v10.html at the root of this distribution.  By
;; using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.  You must not remove this notice, or any
;; other, from this software.
;; Contributors:
;; Brenton Ashworth


(ns com.georgejahad.difform
  (:import name.fraser.neil.plaintext.diff_match_patch$Operation
           name.fraser.neil.plaintext.diff_match_patch)
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint])
  (:use clojure.walk))

(defn- str-comparator [x y]
  (compare (str x) (str y)))

(defn- sort-map [m]
  (into (sorted-map-by str-comparator) m))

(defn- sort-set [s]
  (into (sorted-set-by str-comparator) s))

(defn- s-form [f]
  (if (map? f)
    (sort-map f)
    (if (set? f)
      (sort-set f)
      f)))

(defn sort-form [f]
  (postwalk s-form f))

(def diff-markers
     {diff_match_patch$Operation/EQUAL " "
      diff_match_patch$Operation/INSERT "+"
      diff_match_patch$Operation/DELETE "-"})

(defn- print-diff [d]
  (let [m (diff-markers (.operation d)) ]
    (println (str " " m) (s/replace (.trim (.text d))
                                    #"\n" (str "\n " m " ")))))
(defn canonical-form [f]
  (with-out-str (pprint/pprint (sort-form f))))

(defn difform [x y]
  (let [diffs (.diff_main (diff_match_patch.)
                          (canonical-form x)
                          (canonical-form y))]
    (doseq [d diffs] (print-diff d))))

(defn clean-difform [x y]
  (let [dmp (diff_match_patch.)
        diffs (.diff_main dmp
                          (canonical-form x)
                          (canonical-form y))]
    (do (.diff_cleanupSemantic dmp diffs)
        (doseq [d diffs] (print-diff d)))))
