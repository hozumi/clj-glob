(ns org.satta.glob
  (:require [clojure.java.io :as io :only [file]]
	    [clojure.set :as set :only [union]])
  (:import [java.io File]))

(defn- case-helper [c ignore-case?]
  (let [i (int c)
	d (- (int \a) (int \A))]
    (cond
     (not ignore-case?)  c
     (<= (int \A) i (int \Z)) (str "[" c "|" (char (+ i d)) "]")
     (<= (int \a) i (int \z)) (str "[" c "|" (char (- i d)) "]")
     :else c)))

(defn- glob->regex
  "Takes a glob-format string and returns a regex."
  [s ignore-case? dot-fair?]
  (if (or (= ".." s) (= "." s) (= "~" s))
    s
    (loop [st s
	   re ""
	   curly-depth 0]
      (let [c (if-let [c (first st)] (char c) nil)
	    j (if-let [j (second st)] (char j) nil)
	    nex (rest st)]
	(cond
	 (= c nil) (re-pattern (str (if (or dot-fair? (= \. (first s)))
				      "" "(?=[^\\.])") re))
	 (= c \\) (recur (rest nex) (str re c j) curly-depth)
	 (= c \/) (recur nex (str re (if (= \. j) c "/(?=[^\\.])")) curly-depth)
	 (= c \*) (recur nex (str re "[^/]*") curly-depth)
	 (= c \?) (recur nex (str re "[^/]") curly-depth)
	 (= c \{) (recur nex (str re \() (inc curly-depth))
	 (= c \}) (recur nex (str re \)) (dec curly-depth))
	 (and (= c \,) (< 0 curly-depth)) (recur nex (str re \|) curly-depth)
	 (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur nex (str re \\ c) curly-depth)
	 :else (recur nex (str re (case-helper c ignore-case?)) curly-depth))))))

(defn- abs-path?
  "Returns true if the specified path is absolute, false otherwise."
  [path]
  (or
    (= \/ (first path))      ; /home/qertoip
    (and (= \: (second path))   ; c:/windows
	 (= \\ (File/separatorChar))))) ;windows?

(defn- start-dir
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
  [s]
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

;;  {0 [:a :b]        {0 [:g]         {0 [:a :b :g]
;;   1 [:c :d :e]  +   2 [:h :i]   =>  1 [:c :d :e]
;;   2 [:f]})          3 [:j]}         2 [:f :h :i]
;;                                     3 [:j]}

;; #<File ./lib> => #<File lib>    "./" looks bad.
(defn- list-files-nodot[dir]
  (map #(io/file (.getName %)) (.listFiles dir)))

(defn- depth-map [dir re]
  (letfn [(inner [dir re]
		 (let [childs (filter #(re-matches re (.getName %)) (.listFiles dir))]
		   (if (= childs ())
		     {1 [dir]}
		     (let [rank-childs (reduce val-merge 
					       (map #(inner % re) childs))]
		       (assoc rank-childs (inc (apply max (keys rank-childs))) [dir])))))]
    (let [childs (if (= (.getPath dir) ".")
		   (list-files-nodot dir)
		   (.listFiles dir))
	  childs (filter #(re-matches re (.getName %)) childs)]
      (if (= childs ())
	{1 [dir]}
	(let [rank-childs (reduce val-merge 
				  (map #(inner % re) childs))]
	  (if (= (.toString re) "(?=[^\\.])[^/]*[^/]*")
	    (assoc rank-childs (inc (apply max (keys rank-childs))) [dir])
	    rank-childs))))))

(defn- matching [re dir n]
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
     (filter #(re-matches re (.getName %)) childs))))

(defn- regex-dirs-match [dirs [re n]]
  (mapcat #(matching re % n) dirs))

(defn glob
  "Returns a list of java.io.File instances that match the given glob pattern.
  Ignores dot files unless explicitly included.

  Examples: (glob \"*.{jpg,gif}\") (glob \".*\") (glob \"/usr/*/se*\")"
  [pattern & opt]
  (let [ignore-case? (some #{:i} opt)
	dot-fair? (some #{:a} opt)
	ret-path? (some #{:path} opt)
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
      (map #(.getPath %) result)
      result)))
