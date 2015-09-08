# outlier

This project is an experiment at building data-oriented functions in Clojure aimed at detecting outliers in data sets.

## Usage

The project is built with [Leiningen](https://github.com/technomancy/leiningen).
Once downloaded, run a lein deps then a lein repl.

## Example

    user=> (ns testing (:use [outlier utils core])
      (:use [incanter core charts io pdf])
      (:use [incanter.stats :exclude (mean median)]))
    testing=> (def csv (load-epochdatevalue-from-csv "SP500-prices.csv" :Date :Close))
    testing=> (def sorted-csv (sort-by :Date csv))
    testing=> (def outs (outliers-median (into [] (map #(:Close %) sorted-csv)) 5  1.5))
    testing=> (view (to-datatset outs))
    testing=> (def chart (time-series-plot :Date :Close :data (to-dataset sorted-csv)))
    testing=> (map #(doto chart (add-pointer (:Date (nth sorted-csv (:idx %))) (:Close (nth sorted-csv (:idx %))) :text "Outlier")) outs)
    testing=> (view chart)

## Updates

### 2015.09.08

Updated to Leiningen 2.0, Clojure 1.7, Incanter 1.5.6

## License

Copyright (C) 2011-2015 Artek Consulting
Distributed under the Eclipse Public License, the same as Clojure.
