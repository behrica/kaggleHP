(ns kaggle

  (:require [nextjournal.clerk :as clerk]
            [tablecloth.api :as tc]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.hashing :as h]
            [weavejester.dependency :as dep]))


(comment
  (clerk/clear-cache!)
  (clerk/serve! {:browse? true})
  (clerk/serve! {:watch-paths ["notebooks" "src"]}))

(comment
  (def parsed (h/parse-file "src/kaggle.clj"))
  (def analyzed
    (h/build-graph parsed))
  (dep/transitive-dependencies (:graph analyzed) 'kaggle/result)
  (def hashes
    (h/hash analyzed))

  (get hashes 'kaggle/pipe-fn)
  (clerk/hash+store-in-cas! load-hp-data)

  :ok)

;; (clerk/eval-string "mean-loss")


(defn load-hp-data [file]
  (println "load file : " file)
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






(require '[scicloj.ml.core :as ml]
         '[scicloj.ml.metamorph :as mm]
         '[scicloj.ml.dataset :as ds])



(def train-data (load-hp-data "train.csv.gz"))

(def splits
  (->
   train-data
   (ds/split->seq :kfold)))

(def pipe-fn
  (ml/pipeline
   (mm/replace-missing [:BsmtCond :PoolQC] :value :NA)
   (mm/select-columns [:OverallQual :GarageCars :BsmtCond
                       :OverallCond
                       :GrLivArea :1stFlrSF :2ndFlrSF :TotalBsmtSF
                       :GarageArea :Neighborhood :YearBuilt
                       :ExterQual

                       :SalePrice])
   (fn [ctx]
     (assoc ctx :metamorph.ml/full-ds train-data))
   (mm/transform-one-hot [:OverallQual :GarageCars :Neighborhood
                          :BsmtCond :PoolQC
                          :OverallCond
                          :ExterQual] :full)

   (mm/set-inference-target :SalePrice)
   {:metamorph/id :model}
   
   (mm/model {:model-type :smile.regression/gradient-tree-boost
                :max-depth 20
                :max-nodes 10
                :node-size 8
                :trees 1000})))




(def result
  (ml/evaluate-pipelines [pipe-fn] splits ml/rmse :loss {:evaluation-handler-fn (fn [m] (dissoc m :pipe-fn :metric-fn))}))


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
