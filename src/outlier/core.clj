(ns outlier.core
	(:require outlier.utils)
	(:require clojure.algo.generic.math-functions))

(defrecord Outlier [idx value comparer stddev diff])

(defn- outlier-median?
	"Takes a collection of 'size' items around a middle point.
	Calculates the number of stddev from middle point to set's median.
	If more than threshold, then returns true."
	[sample threshold]
	(let [m (outlier.utils/median sample)
				s (outlier.utils/stddev-median sample)
				value (nth sample (quot (count sample) 2))
				diff (clojure.algo.generic.math-functions/abs (- m value))
				answer (if (< 0 s) (<= threshold (/ diff s)) false)]
		[answer value m s diff]))

(defn- outlier-mean?
	"Takes a collection of 'size' items around a middle point.
	Calculates the number of stddev from middle point to set's mean.
	If more than threshold, then returns true."
	[sample threshold]
	(let [m (outlier.utils/mean sample)
				s (outlier.utils/stddev sample)
				value (nth sample (quot (count sample) 2))
				diff (clojure.algo.generic.math-functions/abs (- m value))
				answer (if (< 0 s) (<= threshold (/ diff s)) false)]
		[answer value m s diff]))

(defn- outlier-mad?
	"Takes a collection of 'size' items around a middle point.
	Calculates the number of mad from middle point to set's median.
	If more than threshold, then returns true."
	[sample threshold]
	(let [m (outlier.utils/median sample)
				mad (outlier.utils/median (map #(clojure.algo.generic.math-functions/abs (- % (outlier.utils/median sample))) sample))
				value (nth sample (quot (count sample) 2))
				diff (clojure.algo.generic.math-functions/abs (- m value))
				answer (if (< 0 mad) (<= threshold (/ diff mad)) false)]
		[answer value m mad diff]))

(defn- outlier-iqr?
	"Takes a collection of 'size' items around a middle point.
	Determines whether the middle point is under the lowerfence or above the upperfence of the set.
	Lowerfence = Q1 - (threshold x iqr).
	Upperfence = Q3 + (threshold x iqr).
	A common value for thresholds is 1.5"
	([sample] (outlier-iqr? sample 1.5))
	([sample threshold]
	(let [iqr (outlier.utils/iqr sample)
				q1 (outlier.utils/q1 sample)
				q3 (outlier.utils/q3 sample)
				lf (- q1 (* threshold iqr))
				uf (+ q3 (* threshold iqr))
				value (nth sample (quot (count sample) 2))]
				(if (> lf value) [true value lf 0 (clojure.algo.generic.math-functions/abs (- lf value))]
					(if (< uf value) [true value uf 0 (clojure.algo.generic.math-functions/abs (- uf value))] [false value 0 0 0])))))

(defn- outliers
	"Helper function for outliers-median and outliers-mean"
	[values sample-size threshold method]
	; sanity checks
	(if (not (coll? values))
		(throw (Exception. "Invalid parameter: 'values' must be a collection. Please check")))
	(if (= 0 (count values))
		(throw (Exception. "Invalid parameters: 'values' collection cannot be empty. Please check")))
	(if (>= 0 sample-size)
		(throw (Exception. "Invalid parameters: 'sample-size' must be > 0. Please check")))
	(if (even? sample-size)
		(throw (Exception. "Invalid parameters: 'sample-size' must be an odd number. Please check")))
	(if (>= sample-size (count values))
		(throw (Exception. "Invalid parameters: 'sample-size' cannot exceed 'values' size. Please check")))
	(if (>= 0 threshold)
		(throw (Exception. "Invalid parameters: 'threshold' must be > 0. Please check")))
	; sets: all the sub-collections we need to analyze (sample-size around each point)
	(let [sets (partition sample-size 1 values)]
		(filter #(not (nil? %))
			(pmap (fn [idx part]
				(let [[out value m s diff] (cond
																			(= "mean" method) (outlier-mean? part threshold)
																			(= "median" method)	(outlier-median? part threshold)
																			(= "mad" method)	(outlier-mad? part threshold)
																			(= "iqr" method)	(outlier-iqr? part threshold)
																			:else (outlier-median? part threshold))]
					(if (= true out)
						; You can return records if you want - but much longer runtime
						;(Outlier. (+ idx (quot (count part) 2)) value m s diff)
						(zipmap [:idx :value :comp :factor :diff] (cons (+ idx (quot (count part) 2)) [value m s diff]))
						))) (iterate inc 0) sets))))

(defn outliers-median
	"Given a collection of sorted values, this function will scan samples of n points around studied point,
	then calculate number of sample stddev from studied point to sample median, and compares this to a given threshold.
	Computation is done in parallel accross available cores.
	Returns a collection of map with the following data:
	- idx: index of outlier in original set of values
	- comp: median of the sample set around outlier
	- factor: std deviation for the sample set around outlier
	- diff: difference between median and outlier
	Of course, we lose a half sample left and right of the received collection.
	Original collection items order won't be changed - the caller must sort it prior to calling this function if necessary.
	Parameters:
	- collection of values
	- sample size (odd)
	- threshold: number of max stddev deviation from point to sample median
	Example:
	(outliers-median (range 100) 5  2)"
	[values sample-size threshold] (outliers values sample-size threshold "median"))

(defn outliers-mean
	"Given a collection of sorted values, this function will scan samples of n points around studied point,
	then calculate number of sample stddev from studied point to sample mean, and compares this to a given threshold.
	Computation is done in parallel accross available cores.
	Returns a collection of map with the following data:
	- idx: index of outlier in original set of values
	- comp: mean of the sample set around outlier
	- factor: std deviation for the sample set around outlier
	- diff: difference between mean and outlier
	Of course, we lose a half sample left and right of the received collection.
	Original collection items order won't be changed - the caller must sort it prior to calling this function if necessary.
	Parameters:
	- collection of values
	- sample size (odd)
	- threshold: number of max stddev deviation from point to sample mean
	Example:
	(outliers-mean (range 100) 5  2)"
	[values sample-size threshold] (outliers values sample-size threshold "mean"))

(defn outliers-mad
	"Given a collection of sorted values, this function will scan samples of n points around studied point,
	then calculate number of sample 'mad' (median absolute deviation) from studied point to sample median,
	and compares this to a given threshold.
	Computation is done in parallel accross available cores.
	Returns a collection of map with the following data:
	- idx: index of outlier in original set of values
	- comp: median of the sample set around outlier
	- factor: median absolute deviation from the median for the sample set around outlier
	- diff: difference between median and outlier
	Of course, we lose a half sample left and right of the received collection.
	Original collection items order won't be changed - the caller must sort it prior to calling this function if necessary.
	Parameters:
	- collection of values
	- sample size (odd)
	- threshold: number of max stddev deviation from point to sample median
	Example:
	(outliers-mad (range 100) 5  2)"
	[values sample-size threshold] (outliers values sample-size threshold "mad"))

(defn outliers-iqr
	"Given a collection of sorted values, this function will scan samples of n points around studied point,
	then calculate whether this point is below the lowerfence (q1 - (threshold * iqr),
	or above the upperfence (q3 + (threshold * iqr)). If any of these is true, then the point is considered an outlier.
	Computation is done in parallel accross available cores.
	Returns a collection of map with the following data:
	- idx: index of outlier in original set of values
	- comp: lowerfence or upperfence (whichever is broken by studied point)
	- factor: not used - 0
	- diff: not used - 0
	Of course, we lose a half sample left and right of the received collection.
	Original collection items order won't be changed - the caller must sort it prior to calling this function if necessary.
	Parameters:
	- collection of values
	- sample size (odd)
	- threshold: number of max stddev deviation from point to sample median
	Example:
	(outliers-iqr (range 100) 5  1.5)"
	[values sample-size threshold] (outliers values sample-size threshold "iqr"))
