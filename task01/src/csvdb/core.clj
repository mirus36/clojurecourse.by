(ns csvdb.core
  (:require [clojure-csv.core :as csv]))

(defn parse-int [int-str]
  (Integer/parseInt int-str))

(def student-tbl (csv/parse-csv (slurp "student.csv")))
(def subject-tbl (csv/parse-csv (slurp "subject.csv")))
(def student-subject-tbl (csv/parse-csv (slurp "student_subject.csv")))

;; (table-keys student-tbl)
;; => [:id :surname :year :group_id]
;;
;; Hint: vec, map, keyword, first
(defn table-keys [tbl]
  (vec (map keyword (first tbl))))


;; (key-value-pairs [:id :surname :year :group_id] ["1" "Ivanov" "1996"])
;; => (:id "1" :surname "Ivanov" :year "1996")
;;
;; Hint: flatten, map, list
(defn key-value-pairs [tbl-keys tbl-record]
  (flatten (map list tbl-keys tbl-record)))


;; (data-record [:id :surname :year :group_id] ["1" "Ivanov" "1996"])
;; => {:surname "Ivanov", :year "1996", :id "1"}
;;
;; Hint: apply, hash-map, key-value-pairs
(defn data-record [tbl-keys tbl-record]
  (apply hash-map (key-value-pairs tbl-keys tbl-record)))

;; (data-table student-tbl)
;; => ({:surname "Ivanov", :year "1996", :id "1"}
;;     {:surname "Petrov", :year "1996", :id "2"}
;;     {:surname "Sidorov", :year "1997", :id "3"})
;;
;; Hint: let, map, next, table-keys, data-record
(defn data-table [tbl]
  (let [table-keys (table-keys tbl)]
    (map
     #(data-record table-keys %)
     (next tbl))))

;; (str-field-to-int :id {:surname "Ivanov", :year "1996", :id "1"})
;; => {:surname "Ivanov", :year "1996", :id 1}
;;
;; Hint: assoc, Integer/parseInt, get
(defn str-field-to-int [field rec]
  (assoc rec field (Integer/parseInt (get rec field))))

(def student (->> (data-table student-tbl)
                  (map #(str-field-to-int :id %))
                  (map #(str-field-to-int :year %))))

(def subject (->> (data-table subject-tbl)
                  (map #(str-field-to-int :id %))))

(def student-subject (->> (data-table student-subject-tbl)
                          (map #(str-field-to-int :subject_id %))
                          (map #(str-field-to-int :student_id %))))

;; (where* student (fn [rec] (> (:id rec) 1)))
;; => ({:surname "Petrov", :year 1997, :id 2} {:surname "Sidorov", :year 1996, :id 3})
;;
;; Hint: if-not, filter
(defn where* [data condition-func]
  (if-not (nil? condition-func)
    (filter #(condition-func %) data)
    data))

;; (limit* student 1)
;; => ({:surname "Ivanov", :year 1998, :id 1})
;;
;; Hint: if-not, take
(defn limit* [data lim]
  (if-not (nil? lim)
    (take lim data)
    data))

;; (order-by* student :year)
;; => ({:surname "Sidorov", :year 1996, :id 3} {:surname "Petrov", :year 1997, :id 2} {:surname "Ivanov", :year 1998, :id 1})
;;
;; Hint: if-not, sort-by
(defn order-by* [data column]
  (if-not (nil? column)
    (sort-by column data)
    data))

;; (join* (join* student-subject :student_id student :id) :subject_id subject :id)
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1}
;;     {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2}
;;     {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]
;;
;; Hint: reduce, conj, merge, first, filter, get
;; Here column1 belongs to data1, column2 belongs to data2.
(defn join* [data1 column1 data2 column2]
  (->> data1
       (reduce (fn [result data1-el]
                 (as-> data2 data2-el
                   (filter #(= (get % column2) (get data1-el column1)) data2-el)
                   (first data2-el)
                   (merge data2-el data1-el)
                   (conj result data2-el)))
               [])))

;; (perform-joins student-subject [[:student_id student :id] [:subject_id subject :id]])
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1} {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]
;;
;; Hint: loop-recur, let, first, next, join*
(defn perform-joins [data joins*]
  (loop [data1 data
         joins joins*]
    (if (empty? joins)
      data1
      (let [[col1 data2 col2] (first joins)]
        (recur (join* data1 col1 data2 col2)
               (next joins))))))

(defn select [data & {:keys [where limit order-by joins]}]
  (-> data
      (perform-joins joins)
      (where* where)
      (order-by* order-by)
      (limit* limit)))

(select student)
;; => [{:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"}]

(select student :order-by :year)
;; => ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})

(select student :where #(> (:id %) 1))
;; => ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})

(select student :limit 2)
;; => ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"})

(select student :where #(> (:id %) 1) :limit 1)
;; => ({:id 2, :year 1997, :surname "Petrov"})

(select student :where #(> (:id %) 1) :order-by :year :limit 2)
;; => ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"})

(select student-subject :joins [[:student_id student :id] [:subject_id subject :id]])
;; => [{:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1} {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Petrov", :year 1997, :student_id 2, :id 2} {:subject "CS", :subject_id 2, :surname "Sidorov", :year 1996, :student_id 3, :id 3}]

(select student-subject :limit 2 :joins [[:student_id student :id] [:subject_id subject :id]])
;; => ({:subject "Math", :subject_id 1, :surname "Ivanov", :year 1998, :student_id 1, :id 1} {:subject "Math", :subject_id 1, :surname "Petrov", :year 1997, :student_id 2, :id 2})
