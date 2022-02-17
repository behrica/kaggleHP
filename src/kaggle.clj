
(ns kaggle

  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as tds]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.hashing :as h]
            [weavejester.dependency :as dep]
            [clojure.string :as str]
            [opencpu-clj.ocpu :as ocpu]
            [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]))

(comment
  (clerk/blob->result)
  (clerk/recompute!)
  (clerk/clear-cache!)
  (clerk/serve! {:browse? true})
  (clerk/serve! {:watch-paths ["notebooks" "src"]}))





(defn load-hp-data [file]
  (println "load a file : " file)
  (-> (tc/dataset file {:key-fn keyword})
      (tc/convert-types (zipmap [:BedroomAbvGr
                                 :BsmtFullBath
                                 :BsmtHalfBath
                                 :Fireplaces
                                 :FullBath
                                 :GarageCars
                                 :HalfBath
                                 :KitchenAbvGr
                                 :OverallCond
                                 :OverallQual
                                 :MoSold
                                 :TotRmsAbvGrd
                                 :MSSubClass]
                                (repeat :string)))
      (tc/rename-columns {
                          :1stFlrSF :FirststFlrSF
                          :2ndFlrSF :SecondFlrSF
                          :3SsnPorch :ThirdsnPorch})))



(def df (load-hp-data "train.csv.gz"))



(defn ->table [a-df]
  (v/html [:pre
           (tc/dataset->str a-df)]))

(defn ->table1 [a-df]
  (->

   (cons (tds/column-names a-df)
         (into [] (tds/rowvecs a-df)))

   clerk/use-headers
   clerk/table))


;;  # The data
^{::clerk/width :full}
(->table df)

^{::clerk/width :full}
(->
 df
 (tc/info)
 (tc/clone)
 ->table)

 ;; # Info on data
;; ## data types
(-> df
    tc/info
    tc/clone
    :datatype
    frequencies)

(-> df
    tc/info
    tc/clone
    (tc/select-columns [:col-name :n-missing])
    (tc/order-by :n-missing :desc)
    ->table)


(defn box-plot [var]
  (clerk/vl
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values (tc/rows df :as-maps)}
    :encoding {:color {:field var :legend nil :type "nominal"}
               :x {:field var :type "nominal"}
               :y {:field :SalePrice
                   :scale {:zero false} :type "quantitative"}}
    :mark {:extent "min-max" :type "boxplot"}}))

(def col-names-categorical
  (-> df
      tc/info
      (tc/select-rows (fn [row] (= :string (:datatype row))))
      :col-name
      vec))

(def col-names-int
  (-> df
      tc/info
      (tc/select-rows (fn [row] (contains?  #{:int16 :int32} (:datatype row))))
      :col-name
      vec))


(clerk/vl
 {::clerk/width :full}
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (vec (tc/rows df :as-maps))}
  :title "Categorical cols"
  :repeat col-names-categorical
  :columns 3
  :spec {
         :width 200
         :height 200
         :encoding {:color {:field {:repeat :repeat} :legend nil :type "nominal"}
                    :x {:field {:repeat :repeat}
                        :type "nominal"
                        :axis {:titleFontSize 20}}
                    :y {:field :SalePrice :scale {:zero false} :type "quantitative"}}
         :mark {:extent "min-max" :type "boxplot"}}})



(clerk/vl
 {::clerk/width :full}
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (vec (tc/rows df :as-maps))}
  :title "Integer  cols"
  :repeat col-names-int
  :columns 3
  :spec {
         :width 200
         :height 200
         :encoding {
                    :x {:field {:repeat :repeat}
                        :type :quantitative
                        :scale {:zero false}
                        :axis {:titleFontSize 20}}
                    :y {:field :SalePrice :scale {:zero false} :type "quantitative"}}
         :mark {:extent "min-max" :type "point"}}})


(clerk/vl
 {::clerk/width :full}
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values
         (-> df
          (tc/select-columns [:YearBuilt :YearRemodAdd :YrSold :SalePrice])
          (tc/rows :as-maps)
          vec)}
  :repeat [:YearBuilt :YearRemodAdd :YrSold]
  :columns 3
  :spec {
         :width 200
         :height 200
         :encoding {
                    :x {:field {:repeat :repeat}
                        :type :quantitative
                        :scale {:zero false}
                        :axis {:titleFontSize 20}}
                    :y {:field :SalePrice
                        :scale {:zero false}
                        :type "quantitative"}}
         :mark { :type "point"}}})


;;  # PPS

;; (def base-url "https://my-container-app.livelybay-00debe37.westeurope.azurecontainerapps.io")
(def base-url "http://localhost:8080")

(def re #"/")

(defn r-object [library function params]
  (let [resp (ocpu/object base-url :library library  :R function params)]
    (when (>  (:status resp) 201) (throw (ex-info "error" resp)))
    (-> resp
        :result
        first
        (str/split re)
        (nth 3))))

(defn r-graph [library function params]
  (-> (ocpu/object base-url :library library  :R function params)
      :result
      (nth 2)))



(defn calc-pps [df]
  (println "calc pps")
  (-> df
    ;; (tc/select-columns candidate-features)
    (tc/write-csv! "/tmp/out.csv"))
  (let [
         ;; last-run  #inst "2022-02-16T20:42:44.871-00:02"
        clean-data
        (as-> "/tmp/out.csv" _
          (r-object "readr"   "read_csv"     {:file {:file _}})
          ;; (r-object "janitor" "clean_names"  {:dat  _})
          (r-object "dplyr"   "mutate_if"    {".tbl" _
                                              ".predicate" "is.character"
                                              ".funs" "as.factor"}))


        pps-graph
        (r-graph "ppsr" "visualize_pps"
                 {:df clean-data
                  :y "'SalePrice'"
                  :do_parallel  true})
        pps-score
        (r-object "ppsr" "score_predictors"
                  {:df clean-data
                   :y "'SalePrice'"
                   :do_parallel true})]

        

    {:png (:result (ocpu/session base-url pps-graph :png))
     :scores (:result (ocpu/session base-url (str "/ocpu/tmp/" pps-score "/R/.val" ) :json))}))

(def pps (calc-pps df))

(with-open [in (io/input-stream (:png pps))]
 (ImageIO/read in))


(def top-x-pps
  (->> pps
       :scores
       (sort-by :pps)
       reverse
       (drop 1)
       (take 5)
       (map (comp keyword :x))))







(defn calc-pps-matrix [df]
  (println "calc pps matrix")
  (-> df
    ;; (tc/select-columns candidate-features)
    (tc/write-csv! "/tmp/out.csv"))
  (let [
        last-run  #inst "2022-02-16T20:42:44.871-00:02"
        clean-data
        (as-> "/tmp/out.csv" _
          (r-object "readr"   "read_csv"     {:file {:file _}})
          (r-object "dplyr"   "mutate_if"    {".tbl" _
                                              ".predicate" "is.character"
                                              ".funs" "as.factor"}))
        pps-graph
        (r-graph "ppsr" "visualize_pps"
                 {:df clean-data
                  :do_parallel true})
        pps-score
        (r-object "ppsr" "score_df"
                  {:df clean-data
                   :do_parallel true})]



    {:png (:result (ocpu/session base-url pps-graph :png))
     :scores (:result (ocpu/session base-url (str "/ocpu/tmp/" pps-score "/R/.val" ) :json))}))

(def pps-top-x (calc-pps-matrix (tc/select-columns df top-x-pps)))

(with-open [in (io/input-stream (:png pps-top-x))]
 (ImageIO/read in))



(require '[scicloj.ml.core :as ml]
         '[scicloj.ml.metamorph :as mm]
         '[scicloj.ml.dataset :as ds])


(def train-data (load-hp-data "train.csv.gz"))

(def splits
  (->
   train-data
   (ds/split->seq :kfold)))

(def cat-features (clojure.set/intersection
                   (into #{} col-names-categorical)
                   (into #{} top-x-pps)))

(def numeric-features (clojure.set/intersection
                       (into #{} col-names-int)
                       (into #{} top-x-pps)))


(def pipe-fn
  (ml/pipeline
   (mm/select-columns
    (concat cat-features numeric-features [:SalePrice]))
   (mm/replace-missing cat-features :value :NA)
   (mm/replace-missing numeric-features :midpoint)


   (fn [ctx]
     (assoc ctx :metamorph.ml/full-ds train-data))
   (mm/transform-one-hot cat-features  :full)

   (mm/set-inference-target :SalePrice)

   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/random-forest
              ;; :max-depth 20
              ;; :max-nodes 10
              ;; :node-size 8
              :trees 1000})))



(defn train []
  (println "Train")
  (ml/evaluate-pipelines [pipe-fn pipe-fn] splits ml/rmse
                         :loss))

(def result (train))


(def mean-loss
  (-> result first first :test-transform :mean))


(def best-pipe-fn (-> result first first :pipe-fn))

(def trained-ctx
  (pipe-fn {:metamorph/data train-data
            :metamorph/mode :fit}))

(def test-data
  (load-hp-data "test.csv.gz"))

(def df-test
  (-> test-data
   (tc/add-column :SalePrice 0)))

(def test-ctx
  (pipe-fn
   (assoc trained-ctx
          :metamorph/data df-test
          :metamorph/mode :transform)))



(def submission (->  (ds/select-columns df-test [:Id])
                     (ds/add-column :SalePrice (-> test-ctx :metamorph/data :SalePrice))))


(ds/write-csv! submission "submission.csv")



(comment
  (def times
    (doall
     (repeatedly 50 (fn [] (clerk/time-ms (clerk/show! "src/kaggle.clj"))))))

  :ok)
