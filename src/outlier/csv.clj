(ns outlier.csv 
  (import com.csvreader.CsvReader)) 
 
(defn- record-seq [filename] 
  (let [csv (CsvReader. filename) 
        read-record (fn []  
                      (when (.readRecord csv)  
                        (into [] (.getValues csv))))] 
    (take-while (complement nil?) (repeatedly read-record)))) 
 
(defn csv-seq 
  "Return a lazy sequence of records (maps) from CSV file. 
  With header map will be header->value, otherwise it'll be position->value." 
  ([filename] (csv-seq filename false)) 
  ([filename headers?] 
   (let [records (record-seq filename) 
         ;headers (if headers? (first records) (range (count (first records))))]
				 headers (map #(keyword %) (if headers? (first records) (range (count (first records)))))]
    (map #(zipmap headers %) (if headers? (rest records) records)))))
