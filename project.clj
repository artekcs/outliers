(defproject outlier "2.0.1-SNAPSHOT"
  :description "This project is an experiment at building data-oriented functions in Clojure aimed at detecting outliers in data sets"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                [org.clojure/algo.generic "0.1.2"]
								[net.sourceforge.javacsv/javacsv "2.0"]
								[incanter "1.5.6"]
								[clj-time "0.11.0"]]
	:dev-dependencies [[org.clojure/clojure "1.7.0"]
										[swank-clojure "1.4.3"]
										[jline "1.0.0"]]
	:repositories {"clojure-releases" "http://build.clojure.org/releases"}
)
