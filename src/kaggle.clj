
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

(def x (+ 1 1))

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
      ;; (tc/head 1000)

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

;; (-> train-data
;;     (tc/select-columns
;;      (concat cat-features numeric-features [:SalePrice]))
;;     (tc/replace-missing cat-features :value :NA)
;;     (tc/replace-missing numeric-features :midpoint))




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
     (repeatedly 10 (fn [] (clerk/time-ms (clerk/show! "src/kaggle.clj"))))))

  :ok)

(comment
  (->
   (h/parse-clojure-string "(+ 1 1)")
   :blocks
   first))

(def parsed
  (repeatedly  10 (fn [] (clerk/parse-file "src/kaggle.clj"))))


(def gs
  (doall
   (repeatedly 10 (fn []
                    (->
                     (clerk/parse-file "src/kaggle.clj")
                     (h/build-graph))))))

(map hash gs)



(def g
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)))


(def hashes
  (doall
   (repeatedly
    50
    (fn []
      (let [
            hs (h/hash g)
            h (get hs 'kaggle/r-object)]
        {:g g
         :hs hs
         :h h
         :ai (-> g :->analysis-info (get 'kaggle/r-object))})))))

(map :h hashes)

(->>
 (group-by :h hashes)
 vals
 distinct
 (map first)
 (map :ai))


 

(->>
 hashes
 (map #(->
        %
        :ai
        (h/hash-codeblock (:hs %)))))

(->>
 hashes
 (map #(->
        %
        :hs))
 (map #(map vec %))
 (apply concat)
 distinct
 (map #(hash-map :k (second %) :v (first %)))
 (sort-by :k))

(def all-hss
  (->>
   hashes
   (map #(->
          %
          :hs))))

(def all-hs
  (->>
   hashes
   (map #(->
          %
          :h))))

(map #(get % 'kaggle/r-object) all-hss)

(= (-> all-hs (nth 8))
   (-> all-hs (nth 9)))

(clojure.data/diff
 (-> all-hs (nth 8))
 (-> all-hs (nth 9)))

(-> hashes first :g)


(count all-hs)



;; => ("(defn r-object [library function params] (let [resp (ocpu/object base-url :library library :R function params)] (when (> (:status resp) 201) (throw (ex-info \"error\" resp))) (-> resp :result first (str/split #\"/\") (nth 3))))"
;;     "(defn r-object [library function params] (let [resp (ocpu/object base-url :library library :R function params)] (when (> (:status resp) 201) (throw (ex-info \"error\" resp))) (-> resp :result first (str/split #\"/\") (nth 3))))"
;;     "(defn r-object [library function params] (let [resp (ocpu/object base-url :library library :R function params)] (when (> (:status resp) 201) (throw (ex-info \"error\" resp))) (-> resp :result first (str/split #\"/\") (nth 3))))")


 
 

(def hashes
  (doall
   (repeatedly
    10
    (fn []
      (->
       (clerk/parse-file "src/kaggle.clj")
       (h/build-graph)
       h/hash

       (select-keys deps))))))

(map #(get % (nth (vec deps) 6)) hashes)

(def hashed-deps (into #{} (map the-hash) deps))

(h/my-hash
 (pr-str (conj hashed-deps (:form code-block))))

code-block

(map the-hash deps)

(defn hash-it []
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)))

(def deps
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)
   :graph
   :dependencies
   (get 'kaggle/r-object)))

(->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)
   :graph
   :dependencies
   (get 'kaggle/r-object))
;; => #{clojure.core/ex-info
;;      clojure.string/split
;;      kaggle/base-url
;;      clojure.core/first
;;      clojure.lang.RT/nth
;;      opencpu-clj.ocpu/object
;;      clojure.lang.Numbers/gt}
;;

(def code-block
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)
   :->analysis-info
   (get 'kaggle/r-object)))

(def code-block
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)
   :->analysis-info
   (get 'kaggle/base-url)))

(def the-hash
  (->
   (clerk/parse-file "src/kaggle.clj")
   (h/build-graph)
   h/hash))
   ;; (get 'kaggle/r-object)

(frequencies
 (repeatedly 10000 (fn []

                     (h/hash-codeblock the-hash code-block))))


(map h/find-location deps)
   
(h/find-location 'clojure.core/ex-info)
(h/find-location 'clojure.core/string)

(h/ns->jar (namespace 'opencpu_clj.ocpu/object))

(h/ns->path (namespace 'opencpu_clj.ocpu/object))



(def diff
  (clojure.data/diff (hash-it) (hash-it)))

(first diff)
;; => {kaggle/mean-loss "5drUzrujmC4y2pqxTpypuJG7o48uEj",
;;     kaggle/cat-features "5ds15TEC2YjRHgUSUTfpSFRSACd6S4",
;;     kaggle/pps "5drUtqAJ7PTeZTRdFQtY2og9fT5DwH",
;;     kaggle/train "5ds2v6vSARFpUdV78LVmqEiRLQETGa",
;;     kaggle/calc-pps-matrix "5dqqFqJDKHfRymyfLMmUNzNGXnjL1S",
;;     kaggle/result "5drVT6Ag2Q31gwPfT9NJzhDXFUuxBm",
;;     kaggle/r-object "5dtVdzbVuypuJooshdmG2GNbETupNc",
;;     (ds/write-csv! submission "submission.csv") "5dtUCfNzx6BXN19dUwyMPJwHW4WsPT",
;;     kaggle/submission "5drB2jy8Zu6vpsyrtSe6k2pzpsMXyq",
;;     kaggle/trained-ctx "5drRv4CxpPBBWhMRcgR3JnybC1M3of",
;;     kaggle/pps-top-x "5dsP95ZJ6UzSGCVaYcEdRS9rpXQnmt",
;;     kaggle/pipe-fn "5druNyjJoz9bv9qbkxdWtgzpTRwHyd",
;;     (with-open [in (io/input-stream (:png pps-top-x))] (ImageIO/read in))
;;     "5dsWQqLTB3KnmvA8r9px28UZyfvbo5",
;;     kaggle/top-x-pps "5dsYxrE8E1744JEFo4d7tanqTBHwCj",
;;     kaggle/numeric-features "5dsx9ojz33QXtBb7BHnTGpH15YeLdY",
;;     kaggle/best-pipe-fn "5dsMGAC7E3y6mJmriYBv6MVzt9w5N6",
;;     kaggle/calc-pps "5drWW1jUPGUpavSUtxpQSXuERri7UG",
;;     kaggle/test-ctx "5drmvSp2Yo6RLP4FHQkkeRXDbfycAQ",
;;     (with-open [in (io/input-stream (:png pps))] (ImageIO/read in))
;;     "5drBpt12H5g1Vfh6kQR96fPS2vdcMK"}
;;
;;
(second diff)
;; => {kaggle/mean-loss "5drqUomYTxrmWwjmp2Go4n7Gu3iKvJ",
;;     kaggle/cat-features "5dtDGFMHaX5av4HWWzu3z8fZRkgye8",
;;     kaggle/pps "5dsTCZYxjpe4e9zaUUMd71LNUzK4v3",
;;     kaggle/train "5drCfvDrdiM6Edj3HGj9PWnbvBXZU3",
;;     kaggle/calc-pps-matrix "5dseZQfgQaJWLxzpDWxG8GFrpErm6J",
;;     kaggle/result "5dryghqXB5DFeoC5L1hZTqCD2ciX3y",
;;     kaggle/r-object "5drvVxUsLCq4ABNQomxptq9wBBVz6a",
;;     (ds/write-csv! submission "submission.csv") "5dsyRx8qLD69NosB5uTwDwbdsyUmxd",
;;     kaggle/submission "5dqsPXZ53iEhQdMCHs7zaHJMaAigQ9",
;;     kaggle/trained-ctx "5dqpFHGRHQkYna2A8gXtiYQSmdWWhz",
;;     kaggle/pps-top-x "5dsC8qzZ4A5BYdfVpSZa6MuLr3DfUY",
;;     kaggle/pipe-fn "5dtYeXEtw5Mcm18ZEQx5Er9UUjtJHK",
;;     (with-open [in (io/input-stream (:png pps-top-x))] (ImageIO/read in))
;;     "5drds6yk8tis3HaCSzKfSqZrfRifKL",
;;     kaggle/top-x-pps "5dt5sRZW2CdVEgYNgerSjstRBPNHS7",
;;     kaggle/numeric-features "5dtXRPGHHMPfKKBqMinxse6K1Y1P8R",
;;     kaggle/best-pipe-fn "5dskPJDvkSXH968LHAA5onbtEaZfeG",
;;     kaggle/calc-pps "5duMDTJuGVd4mm691b4AP7vsx5tshm",
;;     kaggle/test-ctx "5dtgi97zuvzMGwemP9XkGCpeGRHmNc",
;;     (with-open [in (io/input-stream (:png pps))] (ImageIO/read in))
;;     "5dtu9ReNuQ6ZSuEMjbmx221SPuF3Wu"}
(def hs
  (doall
   (repeatedly 100
               (fn []
                 (h/sha1-base58 "#{\"5dsgvkMmMLJgogxVktTjrTwjJ4afJa\" (defn r-object [library function params] (let [resp (ocpu/object base-url :library library :R function params)] (when (> (:status resp) 201) (throw (ex-info \"error\" resp))) (-> resp :result first (str/split #\"/\") (nth 3)))) \"5ds5w3peBKGbmKUaxcfpRpGUUjw1Yf\" \"5dtVT9HVuLAa4BCJBWwz9KQ2b9u37n\"}")))))


;; => ("5dt6Bi1CeH3guTNdnCKCid5TB1hgwA"
;;     "5duH7vdgoEpu9E36jc5fxzWJhMJ7Xg"
;;     "5dt6Bi1CeH3guTNdnCKCid5TB1hgwA"
;;     "5duH7vdgoEpu9E36jc5fxzWJhMJ7Xg"
;;     "5duH7vdgoEpu9E36jc5fxzWJhMJ7Xg"
;;     "5ds3nXtgTQ4aUw3xt4KiM41tQbJUsE"
;;     "5duH7vdgoEpu9E36jc5fxzWJhMJ7Xg"
;;     "5dt6Bi1CeH3guTNdnCKCid5TB1hgwA"
;;     "5duH7vdgoEpu9E36jc5fxzWJhMJ7Xg"
;;     "5ds3nXtgTQ4aUw3xt4KiM41tQbJUsE")
