(ns kaggle

  (:require [nextjournal.clerk :as clerk]
            [tablecloth.api :as tc]
            [nextjournal.clerk :as clerk]))


(comment
  (clerk/clear-cache!)
  (clerk/serve! {:browse? true})
  (clerk/serve! {:watch-paths ["notebooks" "src"]}))




(defn load-hp-data [file]
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
  


^{::clerk/width :full}
(->table df)
 


^{::clerk/width :full}
(->
 df
 (tc/info)
 ->table)

(-> df
    tc/info
    :datatype
    frequencies)

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

;; (map box-plot)


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
                        :axis {:titleFontSize 20}}
                    :y {:field :SalePrice :scale {:zero false} :type "quantitative"}}
         :mark {:extent "min-max" :type "point"}}})



(require '[scicloj.ml.core :as ml]
         '[scicloj.ml.metamorph :as mm]
         '[scicloj.ml.dataset :as ds])

(def splits
  (->
   (load-hp-data "train.csv.gz")
   (ds/split->seq :kfold)))

(def trained-ctx
  (pipe-fn {:metamorph/data titanic-train
            :metamorph/mode :fit}))

(def pipe-fn
  (ml/pipeline
   (mm/select-columns [:OverallQual :SalePrice])
   (mm/categorical->number [:OverallQual])
   (ml/def-ctx ctx)
   (mm/set-inference-target :SalePrice)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/random-forest})))

(def result
  (ml/evaluate-pipelines [pipe-fn] splits ml/rmse :loss))






(def best-pipe-fn (-> result first first :pipe-fn))

(def trained-ctx
  (pipe-fn {:metamorph/data (load-hp-data "train.csv.gz")
            :metamorph/mode :fit}))

(def df-test
  (-> (load-hp-data "test.csv.gz")
      (tc/add-column :SalePrice 0)))

(def test-ctx
  (pipe-fn
   (assoc trained-ctx
          :metamorph/data df-test
          :metamorph/mode :transform)))



(def submission (->  (ds/select-columns df-test [:Id])
                     (ds/add-column :SalePrice (-> test-ctx :metamorph/data :SalePrice))))


(ds/write-csv! submission "submission.csv")