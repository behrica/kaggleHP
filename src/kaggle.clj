(ns kaggle

  (:require [nextjournal.clerk :as clerk]
            [tablecloth.api :as tc]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.hashing :as h]
            [weavejester.dependency :as dep]
            [clojure.string :as str]
            [opencpu-clj.ocpu :as ocpu]
            [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]))



(comment

  (def r
    (ocpu/object base-url :library "ppsr" :R "score_df"
                 {:df "iris"} :json))




  (ocpu/object "http://cloud.opencpu.org" :library "stats" :R "rnorm" {:n 10} :json)

  (ocpu/object "https://cloud.opencpu.org" :library "base" :R "seq" {:from 1 :to 5} :json nil))



 

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
                                (repeat :string)))))

(def df (load-hp-data "train.csv.gz"))

(defn ->table [df]
 (->
  (concat [(tc/column-names df)]
          (tc/rows df :as-seqs))
  clerk/use-headers
  clerk/table))
  

;;  # The data
^{::clerk/width :full}
(->table df)
 


^{::clerk/width :full}
(->
 df
 (tc/info)
 ->table)

 ;; # Info on data
;; ## data types
(-> df
    tc/info
    :datatype
    frequencies)

(-> df
    tc/info
    (tc/select-columns [:col-name :n-missing])
    (tc/order-by :n-missing :desc)
    ->table)


(defn box-plot [var]
  (clerk/vl
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values (vec (tc/rows df :as-maps))}
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

(def col-names-int16
  (-> df
      tc/info
      (tc/select-rows (fn [row] (= :int16 (:datatype row))))
      :col-name
      vec))


(clerk/vl
 {::clerk/width :full}
 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
  :data {:values (vec (tc/rows df :as-maps))}
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
  :repeat col-names-int16
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
(def candidate-features [:OverallQual
                         :GarageCars
                         :Neighborhood
                         :BsmtCond
                         :PoolQC
                         :OverallCond
                         :ExterQual
                         :TotRmsAbvGrd
                         :BedroomAbvGr
                         :SalePrice])
(-> df
    ;; (tc/select-columns candidate-features)
    (tc/write-csv! "/tmp/out.csv"))

(defn r-object [library function params]
  (-> (ocpu/object base-url :library library  :R function params)
      :result
      first
      (str/split #"/")
      (nth 3)))

(defn r-graph [library function params]
  (-> (ocpu/object base-url :library library  :R function params)
      :result
      (nth 2)))



;; (nth 2)


(defn render-pps []
  (let [
        last-run  #inst "2022-02-16T14:59:44.871-00:00"
        r
        (as-> "/tmp/out.csv" _
          (r-object "readr"   "read_csv"     {:file {:file _}})
          (r-object "janitor" "clean_names"  {:dat  _})
          (r-object "dplyr"   "mutate_if"    {".tbl" _
                                              ".predicate" "is.character"
                                              ".funs" "as.factor"})


          (r-graph "ppsr"    "visualize_pps" {:df _
                                              :y "'sale_price'"}))]


        

    (:result (ocpu/session base-url r :svg))))
    ;; (with-open [in (io/input-stream (:result png))]
    ;;   (ImageIO/read in))

(def svg-pps (render-pps))

(clerk/html
 svg-pps)

(require '[scicloj.ml.core :as ml]
         '[scicloj.ml.metamorph :as mm]
         '[scicloj.ml.dataset :as ds])



(def train-data (load-hp-data "train.csv.gz"))

(def splits
  (->
   train-data
   (ds/split->seq :kfold)))

(def cat-features [:OverallQual
                   :Neighborhood
                   :GarageCars
                   :ExterQual
                   :BsmtQual
                   :KitchenQual
                   :Alley
                   :FullBath

                   :TotRmsAbvGrd
                   :BedroomAbvGr])

(def pipe-fn
  (ml/pipeline
   (mm/replace-missing cat-features :value :NA)
   (mm/select-columns
    (concat cat-features
            [
             :GrLivArea :TotalBsmtSF
             :GarageArea  :YearBuilt


             :SalePrice]))
   (fn [ctx]
     (assoc ctx :metamorph.ml/full-ds train-data))
   (mm/transform-one-hot cat-features  :full)

   (mm/set-inference-target :SalePrice)
   {:metamorph/id :model}
   
   (mm/model {:model-type :smile.regression/gradient-tree-boost
              :max-depth 20
              :max-nodes 10
              :node-size 8
              :trees 1000})))




(def result
  (ml/evaluate-pipelines [pipe-fn] splits ml/rmse
                         :loss))
                         ;; {:evaluation-handler-fn
                         ;;  (fn [m] (dissoc m
                         ;;                 ;; :pipe-fn
                         ;;                 :metric-fn))}



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
