(ns org.satta.glob
  (:require [clojure.java.io :as io :only [file]]
            [clojure.set :as set :only [union]])
  (:import [java.util.regex Pattern]
           [java.io File]))

(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s ignore-case? dot-fair?]
  (or (#{".." "." "~"} s)
      (loop [st s
             regs [(when ignore-case? "(?i)")
                   (if-not (or dot-fair? (= \. (first s))) "(?=[^\\.])")]
             curly-depth 0]
        (let [c (first st)
              j (second st)
              nex (rest st)]
          (cond
           (= c nil) (re-pattern (apply str regs))
           (= c \\) (recur (rest nex) (conj regs c j) curly-depth)
           (= c \/) (recur nex (conj regs (if (or dot-fair? (= j \.))
                                            c "/(?=[^\\.])")) curly-depth)
           (= c \*) (recur nex (conj regs "[^/]*") curly-depth)
           (= c \?) (recur nex (conj regs "[^/]") curly-depth)
           (= c \{) (recur nex (conj regs \() (inc curly-depth))
           (= c \}) (recur nex (conj regs \)) (dec curly-depth))
           (and (= c \,) (< 0 curly-depth)) (recur nex (conj regs \|) curly-depth)
           (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur nex (conj regs \\ c) curly-depth)
           :else (recur nex (conj regs c) curly-depth))))))

(defn- abs-path?
  "Returns true if the specified path is absolute, false otherwise."
  [path]
  (or
   (= \/ (first path))      ; /home/qertoip
   (and (= \: (second path))   ; c:/windows
        (= \\ (File/separatorChar))))) ;windows?

(defn- ^File start-dir
  "Returns a start directory for recursive traversal as File instance."
  [path]
  (io/file
   (if (abs-path? path)
     (if (= \: (second path))
       (subs path 0 3)        ; c:/
       "/")                   ; /
     ".")))                   ; .

(defn- first-slash
  "Returns index of the first slash, plus 1"
  [^String s]
  (inc (.indexOf s "/")))

(defn- val-merge [m1 m2]
  (let [ks (set/union (set (keys m1))
                      (set (keys m2)))]
    (reduce (fn [m k]
              (let [m1v (get m1 k [])]
                (if-let [m2v (m2 k)]
                  (assoc m k (apply conj m1v m2v))
                  (assoc m k m1v))))
            m1
            ks)))

;;  {1 [:a :b]        {1 [:g]         {1 [:a :b :g]
;;   2 [:c :d :e]  +   3 [:h :i]   =>  2 [:c :d :e]
;;   3 [:f]})          4 [:j]}         3 [:f :h :i]
;;                                     4 [:j]}

;; #<File ./lib> => #<File lib>    "./" looks bad.
(defn- list-files-nodot[^File dir]
  (map (fn [^File f] (io/file (.getName f))) (.listFiles dir)))

(defn- depth-map [^File dir ^Pattern re]
  (letfn [(inner [^File dir re]
                 (let [childs (filter (fn [^File f] (re-matches re (.getName f)))
                                      (.listFiles dir))]
                   (if (= childs ())
                     {1 [dir]}
                     (let [rank-childs (reduce val-merge
                                               (map #(inner % re) childs))]
                       (assoc rank-childs (inc (apply max (keys rank-childs))) [dir])))))]
    (let [childs (if (= (.getPath dir) ".")
                   (list-files-nodot dir)
                   (.listFiles dir))
          childs (filter (fn [^File f] (re-matches re (.getName f))) childs)
          amI?   (or (= (.toString re) "(?=[^\\.])[^/]*[^/]*")
                     (re-matches re (.getName dir)))]
      (if (= childs ())
        (if amI? {1 [dir]} {})
        (let [rank-childs (reduce val-merge
                                  (map #(inner % re) childs))]
          (if amI?
            (assoc rank-childs (inc (apply max (keys rank-childs))) [dir])
            rank-childs))))))

(defn- matching [^Pattern re ^File dir n]
  (cond
   (= re ".")  [dir]
   (= re "..") [(if (= "." (.getPath dir)) ; "./../" looks bad.
                  (io/file "..")
                  (io/file dir ".."))]
   (= re "~")  [(io/file (System/getProperty "user.home"))]
   (<= 0 (.indexOf (.toString re) "[^/]*[^/]*")) ; **
   (let [d-map (depth-map dir re)
         pass  (filter #(<= n (key %)) d-map)]
     (apply concat (vals pass)))
   :else
   (let [childs (if (= (.getPath dir) ".")
                  (list-files-nodot dir)
                  (.listFiles dir))]
     (filter (fn [^File f] (re-matches re (.getName f))) childs))))

(defn- regex-dirs-match [dirs [re n]]
  (mapcat #(matching re % n) dirs))

(defn glob
  "Returns a list of java.io.File instances that match the given glob pattern.
  Ignores dot files unless explicitly included.

  Examples: (glob \"*.{jpg,gif}\") (glob \".*\") (glob \"/usr/*/se*\")"
  [^String pattern & opt]
  (let [ignore-case? (some #{:i} opt)
        dot-fair? (some #{:a} opt)
        ret-path? (some #{:s} opt)
        abs-path? (abs-path? pattern)
        start-dir (start-dir pattern)
        splitted  (.split (if abs-path? (subs pattern (first-slash pattern)) pattern) "/")
        patterns  (map #(glob->regex % ignore-case? dot-fair?) splitted)
        ;;[#"foo" #"bar" #"baz"] -> [[#"foo" 2] [#"bar" 1] [#"baz" 0]]
        indexed-patterns (map #(identity [%1 %2])
                              patterns
                              (range (- (count patterns) 1) -1 -1))
        result   (reduce regex-dirs-match [start-dir] indexed-patterns)]
    (if ret-path?
      (map (fn [^File f] (.getPath f)) result)
      result)))
