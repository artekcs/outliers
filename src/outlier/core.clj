(ns outlier.core
	(:require outlier.utils)
	(:require clojure.contrib.generic.math-functions))

(defrecord Outlier [idx value comparer stddev diff])

(defn- outlier-median?
	"Takes a collection of 'size' items around a middle point.
	Calculates the number of stddev from middle point to set's median.
	If more than threshold, then returns true."
	[sample threshold]
	(let [m (outlier.utils/median sample)
				s (outlier.utils/stddev-median sample)
				value (nth sample (quot (count sample) 2))
				diff (clojure.contrib.generic.math-functions/abs (- m value))
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
				diff (clojure.contrib.generic.math-functions/abs (- m value))
				answer (if (< 0 s) (<= threshold (/ diff s)) false)]
		[answer value m s diff]))

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
																			:else (outlier-median? part threshold))]
					(if (= true out)
						; You can return records if you want - but much longer runtime
						;(Outlier. (+ idx (quot (count part) 2)) value m s diff)
						(zipmap [:idx :value :comp :stddev :diff] (cons (+ idx (quot (count part) 2)) [value m s diff]))
						))) (iterate inc 0) sets))))

(defn outliers-median
	"Given a collection of sorted values, this function will scan samples of n points around studied point,
	then calculate number of sample stddev from studied point to sample median, and compares this to a given threshold.
	Computation is done in parallel accross available cores.
	Returns a collection of map with the following data:
	- idx: index of outlier in original set of values
	- comp: median of the sample set around outlier
	- stddev: std deviation for the sample set around outlier
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
	- stddev: std deviation for the sample set around outlier
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