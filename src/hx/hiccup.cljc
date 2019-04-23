(ns hx.hiccup
  (:require 
    [clojure.walk :as walk]
    [hx.utils :as util]))

(defprotocol IElement
  (-parse-element [el config args] "Parses an element"))

;; we use a multimethod to dispatch on identity so that consumers
;; can override this for custom values e.g. :<> for React fragments
(defmulti parse-element
  (fn [config el & more]
    (identity el))
  :default ::default)

;; if no multimethod for specific el, then apply general parsing rules
(defmethod parse-element
  ::default
  ([config el & args]
   (-parse-element el config args)))

(defn parse [config hiccup]
  (apply parse-element config hiccup))

(defn make-element [config el args]
  ((:create-element config) config el args))

#?(:clj (defn array? [x]
          (coll? x)))

(defn ex [s]
  #?(:clj (Exception. s)
     :cljs (js/Error. s)))

(defn parse-keyword [kw]
  (let [w (name kw)
        [tag & attributes] (clojure.string/split w #"#|\.")]
    (if (some #{"#"} w)
      {:tag tag
       :id (first attributes)
       :classes  (rest attributes)}
      {:tag tag
       :classes attributes})))

(extend-protocol IElement
  nil
  (-parse-element [_ _ _]
    nil)

  #?(:clj Number
     :cljs number)
  (-parse-element [n _ _]
    n)

  #?(:clj String
     :cljs string)
  (-parse-element [s _ _]
    s)

  #?(:clj clojure.lang.PersistentVector
     :cljs PersistentVector)
  (-parse-element [form config _]
    (apply parse-element config form))

  #?(:clj clojure.lang.LazySeq
     :cljs LazySeq)
  (-parse-element [a config b]
    (make-element
     config
     (:fragment config)
     (cons nil (map (partial parse-element config) a))))

  #?(:clj clojure.lang.Keyword
     :cljs Keyword)
  (-parse-element [el config args]
    (let [{:keys [tag id classes]} (parse-keyword el)
          args' (cond-> args
                  ;; If there is some id then insert that 
                  ;; id into arguments
                  (some? id) ((fn [[attrm & others :as args]]
                                (cond
                                  ;; Check if there was some argument map before
                                  ;; and if not prepend new map
                                  (string? attrm) (conj args {:id id}) 
                                  ;; if attribute map already present than update
                                  ;; current map
                                  (map? attrm) 
                                  (concat 
                                    (list (assoc attrm :id id))
                                    others)))) 
                  (some? classes) ((fn [[attrm & others :as args]]
                                     (cond
                                       (string? attrm) 
                                       (conj args {:class classes})
                                       ;;
                                       (map? attrm) 
                                       (let [{c :class} attrm]
                                         (concat 
                                           (list (assoc attrm :class (cond
                                                                       (nil? c) classes
                                                                       (string? c) (conj classes c)
                                                                       (seq? c) (concat c classes))))
                                           others))))))] 
      (make-element config tag args')))

  #?(:clj clojure.lang.AFn
     :cljs function)
  (-parse-element [el config args]
    (make-element config el args))

  #?(:clj Object
     :cljs default)
  (-parse-element [el config args]
    (cond
      ((:is-element? config) el) el

      ((:is-element-type? config) el)
      (make-element config el args)

      ;; handle array of children already parsed
      (and (array? el) (every? (:is-element? config) el))
      el

      (var? el)
      (make-element config
                    (fn VarEl [& args] (apply el args))
                    args)

      :default
      (throw
       (ex (str "Unknown element type " (prn-str (type el))
                       " found while parsing hiccup form: "
                       (.toString el)))))))
