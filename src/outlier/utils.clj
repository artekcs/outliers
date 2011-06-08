(ns outlier.utils
	(:require outlier.csv)
	(:require clojure.contrib.generic.math-functions)
	(:require clojure.contrib.profile))

; debug macro
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; load CSV file
(defn load-datevalue-csv
	"Loads a CSV with a date / decimal value structure into an arraymap,
	convert its text contents to dates and floats and returns it"
	[filename]
	(let [csvlines (outlier.csv/csv-seq filename true)]
		(map #(merge %1 %2) (map #(zipmap [:value] [(Float/valueOf (:value %))]) csvlines) 
		(map #(zipmap [:date] [(. (new java.text.SimpleDateFormat "M/d/y") (parse (:date %)))]) csvlines))))

; load CSV file with EPOCH date (needed by time series chart in Incanter)
(defn load-epochvalue-csv
	"Loads a CSV with an epoch date / decimal value structure into an arraymap,
	convert its text contents to dates and floats and returns it"
	[filename]
	(let [csvlines (outlier.csv/csv-seq filename true)]
		(map #(merge %1 %2) (map #(zipmap [:value] [(Float/valueOf (:value %))]) csvlines) 
		(map #(zipmap [:date] [(. (. (new java.text.SimpleDateFormat "M/d/y") (parse (:date %))) getTime)]) csvlines))))

; median definition
(defn median
	"Returns the median value of a vector of numerical values"
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	; sort
	(let [sorted-values (sort values)]
		(if (odd? (count sorted-values))
			(nth sorted-values (quot (count sorted-values) 2))
			; we take the mean between the 2 middle numbers
			(/ (+ (nth sorted-values (- (quot (count sorted-values) 2) 1)) (nth sorted-values (quot (count sorted-values) 2))) 2))))
	
; mean definition
(defn mean
	"Returns the mean value of a vector of numerical values"
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(/ (apply + values) (count values)))

; stddev definition
(defn stddev
	"Returns the regular, mean-based stddev of a vector of numerical values.
	If you pass true or norhing for the second argument, result is a entire population-based stddev.
	If you pass false for the second argument, result is a sample population-based stddev."
	([values] (stddev values true))
	([values entire?]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(let [m (mean values)
				p (if entire? (count values) (- (count values) 1))]
		(clojure.contrib.generic.math-functions/sqrt (/ (apply + (map #(clojure.contrib.generic.math-functions/sqr (- % m)) values)) p)))))

; stddev-median definition
(defn stddev-median
	"Returns the median-based stddev of a vector of numerical values
	If you pass true or norhing for the second argument, result is a entire population-based stddev.
	If you pass false for the second argument, result is a sample population-based stddev."
	([values] (stddev-median values true))
	([values entire?]
	(let [m (median values)
				p (if entire? (count values) (- (count values) 1))]
		(clojure.contrib.generic.math-functions/sqrt (/ (apply + (map #(clojure.contrib.generic.math-functions/sqr (- % m)) values)) p)))))

; mode definition
(defn mode
	"Returns most frequent value in a given set, in a lazy sequence.
	If several modes, returns them all.
	If no mode, returns nil.
	Inspired by http://stackoverflow.com/questions/1601321/idiomatic-mode-function-in-clojure"
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	; tally items in collection
	(let [tally (apply merge-with + (map (fn [x] {x 1}) values))
				mx (apply max (vals tally))
				modes (map key (filter #(= mx (val %)) tally))]
		(when (< (count modes) (count tally)) modes)))

; filter-indexed
(defn filter-indexed
	"Same as filter, except that it returns the index of found items together with found items.
	(def a (filter-indexed nil? [0 1 8 3 nil 6 2 nil 8 7])) will return ({:idx 4 :value nil} {:idx 7 :value nil}).
	This version is not chunked sequence aware."
	[pred coll]
	(lazy-seq
		(when-let [s (seq coll)]
			(let [ct (count s)]
			(loop [result (transient []) s1 s ix 0]
				(if (>= ix ct)
					(persistent! result)
					(let [f (first s1)]
						(if (pred f)
							(conj! result {:idx ix :value f}))
						(recur result (rest s1) (inc ix)))))))))
