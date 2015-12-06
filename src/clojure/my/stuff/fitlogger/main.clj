(ns my.stuff.fitlogger.main
    (:require [neko.activity :refer [defactivity set-content-view!]]
              [neko.debug :refer [*a]]
              [neko.notify :refer [toast]]
              [neko.resource :as res]
              [neko.find-view :refer [find-view]]
              [neko.threading :refer [on-ui]]
              [neko.ui :refer [config make-ui]]
              [neko.ui.mapping :refer [defelement]]
              [clojure.string :refer [join split-lines split]]
              [clojure.java.io :as io]
              [my.stuff.fitlogger.sebas :as seb])
    (:import (android.widget EditText TextView)
             (android.view ViewGroup)
             (java.util Calendar)
             (java.io File)
             (android.app Activity)
             (android.app DatePickerDialog DatePickerDialog$OnDateSetListener)
             (android.app DialogFragment)
             (android.content.res AssetManager)))

;; We execute this function to import all subclasses of R class. This gives us
;; access to all application resources.
(res/import-all)

(defn notify-from-edit
  "Finds an EditText element with ID ::user-input in the given activity. Gets
  its contents and displays them in a toast if they aren't empty. We use
  resources declared in res/values/strings.xml."
  [activity]
  (let [^EditText input (.getText (find-view activity ::user-input))]
    (toast (if (empty? input)
             (res/get-string R$string/input_is_empty)
             (res/get-string R$string/your_input_fmt input))
           :long)))


(defn get-elmt [activity elmt]
  (str (.getText ^TextView (find-view activity elmt))))

(defn set-elmt [activity elmt s]
  (on-ui (config (find-view activity elmt) :text s)))

(defn trim-keyword [k]
  (let [trimmed (apply str (rest (drop-while #(not= % \/) (str k))))]
    (if (empty? trimmed)
      (str k)
      trimmed)))

(defelement :table-layout
  :classname android.widget.TableLayout
  :inherits :linear-layout)
(defelement :table-row
  :classname android.widget.TableRow
  :inherits :linear-layout)

(defn format-measurements [m]
  (apply conj [:table-row {}]
         (map (fn [[_ v]]
                [:text-view {:text (format "%s" v), :padding 20}])
              m)))

(defn format-listing [lst]
  (apply conj [:table-layout {:id :table}]
         (map (fn [[_ measurements]]
                (format-measurements measurements))
              lst)))

(defn measurement->csv [m]
  (->> (map (fn [[k v]]
            (format "%s" v))
          m)
     (join ", ")))

(defn listing->csv [lst]
  (->> (map (fn [[_ measurements]]
            (format "%s" (measurement->csv measurements)))
          lst)
     (join "\n")))

(defn csv-list->entry [l entry]
  (let [date-key (read-string (entry 0))
        pairs (map #(identity [%1 %2])
                   [:date :weight :bf :water :muscle] entry)]
    (reduce (fn [l [k v]] (assoc-in l [date-key k] (read-string v)))
            l
            pairs)))

(defn csv->listing [entries]
  (reduce (fn [l e] (csv-list->entry l (split e #",")))
          (sorted-map-by >)
          entries))

(def listing (atom (sorted-map-by >)))

(defn index-of-key [key map]
  (count (take-while #(not= key %) (keys map))))

(defn update-table [activity key m]
  (let [idx (index-of-key key m)
        row (format-measurements (m key))]
    (on-ui (.addView (find-view activity :table) (make-ui (*a) row) idx))
    row))

(defn update-ui [activity date-key]
  (update-table activity date-key @listing)
  (set-elmt activity :weight "")
  (set-elmt activity :bf "")
  (set-elmt activity :water "")
  (set-elmt activity :muscle ""))


(def logfile "log5.csv")

(defn write-logfile [activity t]
  (let [file (File. (.getFilesDir activity) logfile)]
    (do (when (not (.exists file))
          (.createNewFile file))
        (with-open [wrt (io/writer file)]
          (.write wrt t)))))

(defn read-logfile [activity]
  (let [file (File. (.getFilesDir activity) logfile)]
    (when (.exists file)
      (with-open [rdr (io/reader file)]
        (slurp rdr)))))

(defn read-logfile-or-default [activity]
  (if-let [log (read-logfile activity)]
    (csv->listing (split-lines log))
    seb/sebas-default))

(defn add-event [activity]
  (let [date-key (try
                   (read-string (get-elmt activity :date))
                   (catch RuntimeException e "Date string is empty!"))]
    (when (number? date-key)
      (swap! listing
             #(reduce (fn [l k] (assoc-in l [date-key k] (read-string (get-elmt activity k))))
                      %
                      [:date :weight :bf :water :muscle]))
      (write-logfile activity (listing->csv @listing))
      (update-ui activity date-key))))

(defn date-picker [activity]
  (proxy [DialogFragment DatePickerDialog$OnDateSetListener] []
    (onCreateDialog [savedInstanceState]
      (let [c (Calendar/getInstance)
            year (.get c Calendar/YEAR)
            month (.get c Calendar/MONTH)
            day (.get c Calendar/DAY_OF_MONTH)]
        (DatePickerDialog. activity this year month day)))
    (onDateSet [view year month day]
      (set-elmt activity :date
                (format "%d%02d%02d" year (inc month) day)))))

(defn show-picker [^Activity activity dp]
  (. dp show (. activity getFragmentManager) "datePicker"))

(defn main-layout [activity]
  [:linear-layout {:orientation :vertical}
   [:linear-layout {:orientation :horizontal}
    [:button {:text "Date",
              :id :date,
              :on-click (fn [_] (show-picker activity
                                            (date-picker activity)))}]
    [:edit-text {:hint "Weight",
                 :id :weight}]
    [:edit-text {:hint "BF%",
                 :id :bf}]
    [:edit-text {:hint "H2O%",
                 :id :water}]
    [:edit-text {:hint "M%",
                 :id :muscle}]]
   [:button {:text "Add log",
             :on-click (fn [_]
                         (add-event activity))}]
   [:scroll-view {}
    (format-listing @listing)]])

;; This is how an Activity is defined. We create one and specify its onCreate
;; method. Inside we create a user interface that consists of an edit and a
;; button. We also give set callback to the button.
(defactivity my.stuff.fitlogger.MainActivity
  :key :main

  (onCreate [this bundle]
    (.superOnCreate this bundle)
    (neko.debug/keep-screen-on this)
    (reset! listing (read-logfile-or-default (*a)))
    (on-ui
     (set-content-view! (*a) (main-layout (*a))))))
