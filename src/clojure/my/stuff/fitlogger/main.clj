(ns my.stuff.fitlogger.main
    (:require [neko.activity :refer [defactivity set-content-view!]]
              [neko.debug :refer [*a]]
              [neko.notify :refer [toast]]
              [neko.resource :as res]
              [neko.find-view :refer [find-view]]
              [neko.threading :refer [on-ui]]
              [neko.ui :refer [config]]
              [clojure.string :refer [join]]
              [clojure.java.io :as io])
    (:import (android.widget EditText TextView)
             (java.util Calendar)
             (java.io File)
             (android.app Activity)
             (android.app DatePickerDialog DatePickerDialog$OnDateSetListener)
             (android.app DialogFragment)))

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
  (apply str (rest (drop-while #(not= % \/) (str k)))))

(defn format-measurements [m]
  (->>
   (map (fn [[k v]]
            (format "%s: %s" (trim-keyword k) v))
          m)
   (join ", ")))

(defn format-listing [lst]
  (->> (map (fn [[_ measurements]]
            (format "%s" (format-measurements measurements)))
          lst)
     (join "\n")))

(def listing (atom (sorted-map)))

(defn update-ui [activity]
  (set-elmt activity ::listing (format-listing @listing))
  (set-elmt activity ::weight "")
  (set-elmt activity ::bf "")
  (set-elmt activity ::water "")
  (set-elmt activity ::muscle ""))

(defn add-event [activity]
  (let [date-key (try
                   (read-string (get-elmt activity ::date))
                   (catch RuntimeException e "Date string is empty!"))]
    (when (number? date-key)
      (swap! listing
             #(reduce (fn [l k] (assoc-in l [date-key k] (read-string (get-elmt activity k))))
                      %
                      [::date ::weight ::bf ::water ::muscle]))
      (update-ui activity))))

(defn date-picker [activity]
  (proxy [DialogFragment DatePickerDialog$OnDateSetListener] []
    (onCreateDialog [savedInstanceState]
      (let [c (Calendar/getInstance)
            year (.get c Calendar/YEAR)
            month (.get c Calendar/MONTH)
            day (.get c Calendar/DAY_OF_MONTH)]
        (DatePickerDialog. activity this year month day)))
    (onDateSet [view year month day]
      (set-elmt activity ::date
                (format "%d%02d%02d" year (inc month) day)))))

(defn show-picker [^Activity activity dp]
  (. dp show (. activity getFragmentManager) "datePicker"))

(def logfile "test.csv")

(defn write-file [activity t]
  (let [file (File. (.getFilesDir activity) logfile)]
    (do (when (not (.exists file))
          (.createNewFile file))
        (with-open [wrt (io/writer file)]
          (.write wrt t)))))

(defn read-file [activity]
  (let [file (File. (.getFilesDir activity) logfile)]
    (when (.exists file)
      (set-elmt activity ::test  (slurp file)))))

(defn main-layout [activity]
  [:linear-layout {:orientation :vertical}
   [:linear-layout {:orientation :horizontal}
    [:button {:text "Date",
              :id ::date,
              :on-click (fn [_] (show-picker activity
                                            (date-picker activity)))}]
    [:edit-text {:hint "Weight",
                 :id ::weight}]
    [:edit-text {:hint "BF%",
                 :id ::bf}]
    [:edit-text {:hint "H2O%",
                 :id ::water}]
    [:edit-text {:hint "M%",
                 :id ::muscle}]]
   [:button {:text "Add log",
             :on-click (fn [_]
                         (add-event activity))}]
   [:button {:text "Write file",
             :on-click (fn [_]
                         (write-file activity "arst"))}]
   [:button {:text "Read file",
             :on-click (fn [_]
                         (read-file activity))}]
   [:text-view {:id ::test}]
   [:text-view {:text (format-listing @listing),
                :id ::listing}]])

;; This is how an Activity is defined. We create one and specify its onCreate
;; method. Inside we create a user interface that consists of an edit and a
;; button. We also give set callback to the button.
(defactivity my.stuff.fitlogger.MainActivity
  :key :main

  (onCreate [this bundle]
    (.superOnCreate this bundle)
    (neko.debug/keep-screen-on this)
    (on-ui
     (set-content-view! (*a) (main-layout (*a)))
     (set-elmt (*a) ::listing (format-listing @listing)))))
