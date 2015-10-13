(ns outlier.utils
	(:require outlier.csv)
	(:require clojure.algo.generic.math-functions))

; debug macro
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; load CSV file
(defn load-datevalue-from-csv
	"Loads an arraymap with a date / value structure from a CSV file,
	convert its text contents to dates and floats and returns it.
	date-col is the date column name (as symobol)
	value-col is the value column name (as symobol)"
	[filename date-col value-col]
	(let [csvlines (outlier.csv/csv-seq filename true)
			 	fmt (new java.text.SimpleDateFormat "yyyy-MM-dd")]
		(. fmt (setTimeZone(java.util.TimeZone/getTimeZone "UTC" )))
		(map #(merge %1 %2) (map #(zipmap [value-col] [(Float/valueOf (value-col %))]) csvlines)
			(map #(zipmap [date-col] [(. fmt (parse (date-col %)))]) csvlines))))
;		(map #(zipmap [date-col] [(. (new java.text.SimpleDateFormat "M/d/y") (parse (date-col %)))]) csvlines))))

; load CSV file with EPOCH date (needed by time series chart in Incanter)
(defn load-epochdatevalue-from-csv
	"Loads an arraymap with an epoch date / value structure from a CSV file,
	convert its text contents to dates and floats and returns it.
	date-col is the date column name (as symobol)
	value-col is the value column name (as symobol)"
	[filename date-col value-col]
	(let [csvlines (outlier.csv/csv-seq filename true)
				fmt (new java.text.SimpleDateFormat "yyyy-MM-dd")]
		(. fmt (setTimeZone(java.util.TimeZone/getTimeZone "UTC" )))
		(map #(merge %1 %2) (map #(zipmap [value-col] [(Float/valueOf (value-col %))]) csvlines)
			(map #(zipmap [date-col] [(. (. fmt (parse (date-col %))) getTime)]) csvlines))))

; median definition
(defn median
	"Returns the median value (or second quartile, or Q2) of a collection of numerical values"
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

; q1 definition
(defn q1
	"Returns the first quartile (Q1) of a collection of numerical values.
	The method used here doesn't include the set median in the two halves."
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
		; sort
		(let [sorted-values (sort values)]
			; actually, we don't care if the set has odd or even members, we always exclude the median.
			(median (take (quot (count sorted-values) 2) sorted-values))))

; q3 definition
(defn q3
	"Returns the third quartile (Q3) of a collection of numerical values.
	The method used here doesn't include the set median in the two halves."
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
		; sort
		(let [sorted-values (sort values)]
			; actually, we don't care if the set has odd or even members, we always exclude the median.
			(median (take-last (quot (count sorted-values) 2) sorted-values))))

; iqr definition
(defn iqr
	"Returns the Interquartile Range (IQR) of a collection of numerical values.
	IQR is the difference between first quartile (Q1) and the third one (Q3)."
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(- (q3 values) (q1 values)))

; mean definition
(defn mean
	"Returns the mean value of a collection of numerical values"
	[values]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(/ (apply + values) (count values)))

; stddev definition
(defn stddev
	"Returns the regular, mean-based stddev of a vector of numerical values.
	If you pass true or nothing for the second argument, result is a entire population-based stddev.
	If you pass false for the second argument, result is a sample population-based stddev (Bessel's Correction)."
	([values] (stddev values true))
	([values entire?]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(let [m (mean values)
				p (if entire? (count values) (- (count values) 1))]
		(clojure.algo.generic.math-functions/sqrt (/ (apply + (map #(clojure.algo.generic.math-functions/sqr (- % m)) values)) p)))))

; stddev-median definition
(defn stddev-median
	"Returns the median-based stddev of a vector of numerical values
	If you pass true or nothing for the second argument, result is a entire population-based stddev.
	If you pass false for the second argument, result is a sample population-based stddev (Bessel's Correction)."
	([values] (stddev-median values true))
	([values entire?]
	(let [m (median values)
				p (if entire? (count values) (- (count values) 1))]
		(clojure.algo.generic.math-functions/sqrt (/ (apply + (map #(clojure.algo.generic.math-functions/sqr (- % m)) values)) p)))))

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
