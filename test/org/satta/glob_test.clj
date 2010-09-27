(ns org.satta.glob-test
  (:use [org.satta.glob] :reload-all)
  (:use [clojure.test])
  (:require [clojure.java.io :as io :only [file]])
  (:import [java.io File]))

;; glob->regex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def glob->regex #'org.satta.glob/glob->regex)

(defn- matches?
  "Whether a glob pattern matches a path string"
  [pattern path & opt]
  (let [ignore-case? (some #{:i} opt)
	dot-fair? (some #{:a} opt)]
  (not (nil? (re-matches (glob->regex pattern ignore-case? dot-fair?)
			 path)))))

(deftest test-glob->regex
  (testing "General matching"
    (are [pattern path] (matches? pattern path)
	 "abcd"		"abcd"		
	 "a*d"		"abcd"		
	 "a??d"		"abcd"		
	 "*"		"abcd"		
	 "foo.bar"	"foo.bar"	
	 "foo.*"	"foo.bar"	
	 "*.bar"	"foo.bar"	
	 "*foo.*"	"foo.bar"	     
	 "foo*"		"foo.bar"	
	 "*.{bar,baz}"	"foo.bar"	
	 "*.{bar,baz}"	"foo.baz"	
	 "{foo,bar}"	"foo"		
	 "{foo,bar}"	"bar"		
	 "foo,bar"	"foo,bar"	
	 "*,*"		"foo,bar"	
	 "foo\\*bar"	"foo*bar"	
	 ".()|+^$@%"	".()|+^$@%"	
	 "foo/bar.*"	"foo/bar.baz"	
	 "foo/*.baz"	"foo/bar.baz"	
	 "*/*"		"foo/bar.baz"	
	 ".*.foo"	".bar.foo"	     
	 ".*bar.foo"	".bar.foo"	
	 ".*/bar"	".foo/bar"	
	 "foo/.*"	"foo/.bar"	     
	 "foo.[ch]"	"foo.c"	
	 "foo.[ch]"	"foo.h"	
	 "foo.[c-h]"	"foo.c"	
	 "foo.[c-h]"	"foo.e"	
	 "foo.[c-h]"	"foo.h")
    (is (matches? "ABCD" "abcd" :i))
    (is (matches? "*bar.foo" ".bar.foo" :a)))
  (testing "Dot files ignored"
    (are [pattern path] (not (matches? pattern path))
         "*"           ".foo"
         "*.*"         ".foo"
         "*.foo"       ".bar.foo"
         "*.bar.foo"   ".bar.foo"
         "foo/*"       "foo/.bar"
         "?bar.foo"    ".bar.foo"))
  (testing "Character ranges that shouldn't match"
    (are [pattern path] (not (matches? pattern path))
         "foo.[ch]"    "foo.a"
         "foo.[ch]"    "foo.d"
         "foo.[c-h]"   "foo.b"
         "foo.[c-h]"   "foo.i")))

(in-ns 'org.satta.glob)
(use '[clojure.test])

(deftest test-abs-path?
  (testing "abs-path? function test"
    (are [expected path] (= expected (abs-path? path))
	      true "/"
	      true "/user/home"
	      false "*"
	      false "~"
	      false "lib")
    (if (= \\ (File/separatorChar))
      (testing "abs-path? test on windows"
	(are [expected path] (= expected (abs-path? path))
		  true "c:/clojure"
		  true "d:/*"
		  true "/home"
		  false "~"
		  false "lib")))))

(deftest test-start-dir
  (are [expected path] (= expected (.getPath (start-dir path)))
	    "/"  "/usr/bin/*"
	    "."  "l*/*"
	    "."  "../"))

;; glob ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-ns 'org.satta.glob-test)

(defn mock-fs
  "Takes a tree of vectors and returns a minimal, fake file/dir hierarchy.
  Only getName and listFiles will return reliable (fake) results."
  [node & [path]]
  (if (vector? node)
    (let [dir-name (first node)
	  new-path (if path
		     (str path (if (not= path "/") "/") dir-name)
		     dir-name)
	  childs (into-array File (map #(mock-fs % new-path) (rest node)))]
      (if path
	(proxy [File] [path dir-name]
	  (listFiles [] childs))
	(proxy [File] [dir-name]
	  (listFiles [] childs))))
    (proxy [File] [path node]
      (listFiles [] nil))))

(deftest test-mock-fs
  (let [fs (mock-fs ["fiz" ["foo" "subfoo1" "subfoo2"] "bar" "baz"])]
    (is (= fs (File. "fiz")))
    (is (= (seq (.listFiles fs))
           [(File. "fiz/foo") (File. "fiz/bar") (File. "fiz/baz")]))
    (is (= (seq (.listFiles (first (.listFiles fs))))
           [(File. "fiz/foo/subfoo1") (File. "fiz/foo/subfoo2")]))
    (is (= (map #(.getName %) (.listFiles fs))
           ["foo" "bar" "baz"]))))

(def test-root "org.satta.glob-test")

(def shallow-fs
     (mock-fs
      [test-root "cat.jpg" "dog.jpg" "lol.gif" ".hidden"]))

(def deep-fs
     (mock-fs
      [test-root
       ["usr"
        ["bin" "awk" "bzip" "cd" "diff" "sed" "segedit" "xargs"]
        ["lib" "pam" "samba" "java"]
        ["sbin" "arp" "fdisk" "sendmail" "tcpdump"]
        ["share"
         ["man" "man1" "man2" "man3"]]]]))

(defn glob*
  "Glob with a fake filesystem. Also returns seq of file names rather
  than File instances."
  [pattern start-dir func]
  (binding [io/file (fn [_] start-dir)]
    (map func (glob pattern))))

(deftest test-glob-in-mock
  (testing "Simple, shallow (single-level) matching"
    (are [pattern files] (= (glob* pattern shallow-fs #(.getName %)) files)
         "*"            ["cat.jpg" "dog.jpg" "lol.gif"]
         "*.*"          ["cat.jpg" "dog.jpg" "lol.gif"]
         ".*"           [".hidden"]
         "*.jpg"        ["cat.jpg" "dog.jpg"]
         "*.{jpg,gif}"  ["cat.jpg" "dog.jpg" "lol.gif"]))
  (testing "Deep (multi-level) matching"
    (are [pattern files] (= (glob* pattern deep-fs #(.getName %)) files)
         "*"         ["usr"]
         "usr/*"     ["bin" "lib" "sbin" "share"]
         "usr/*/se*" ["sed" "segedit" "sendmail"]
         "*/*/a*"    ["awk" "arp"]
         "*/*/*/*"   ["man1" "man2" "man3"]))
  (testing "Deep (multi-level) matching"
    (are [pattern files] (= (glob* pattern deep-fs #(.getPath %))
			    (map #(str test-root "/" %) files))
         "*"         ["usr"]
         "usr/*"     ["usr/bin" "usr/lib" "usr/sbin" "usr/share"]
         "usr/*/se*" ["usr/bin/sed" "usr/bin/segedit" "usr/sbin/sendmail"]
         "*/*/a*"    ["usr/bin/awk" "usr/sbin/arp"]
         "*/*/*/*"   ["usr/share/man/man1" "usr/share/man/man2" "usr/share/man/man3"])))

(defn rm-rf [f & [silently]]
  (if (.isDirectory f)
    (map #(rm-rf % silently) (.listFiles f)))
  (io/delete-file f silently))

(defn mk-temp-file
  [f]
  (doto f
    (.createNewFile)
    (.deleteOnExit)))

(defn mk-temp-dir
  [f]
  (doto f
    (.mkdir)
    (.deleteOnExit)))

(defn mk-tmpfs
  "Takes a tree of vectors and make directories and files under parent."
  [parent node]
  (if (vector? node)
    (let [d (io/file parent (first node))]
      (mk-temp-dir d)
      (doall (map #(mk-tmpfs d %) (rest node)))
      d)
    (mk-temp-file (io/file parent node))))

(def shallow-fs1
     ["shallow" "cat.jpg" "dog.jpg" "lol.gif" ".hidden"])

(def deep-fs1
     ["deep"
      ["usr"
       ["bin" "awk" "bzip" "cd" "diff" "sed" "segedit" "xargs"]
       ["lib" "pam" "samba" "java"]
       ["sbin" "arp" "fdisk" "sendmail" "tcpdump"]
       ["share"
	["man" "man1" "man2" "man3"]]]])

(def deep-fs2
     ["deep2"
      ["src"
       ["org" "c.clj"]
       "b.clj"]
      ["lib" "A.jar"
       ["dev" "B.jar"]
       ["lab" ["lob"]]]
      "a.clj"])

(deftest test-glob
  (let [dir-name (str (System/getProperty "java.io.tmpdir")
		      "org.satta.glob-test/")
	dir  (io/file dir-name)]
    (rm-rf dir :silently)
    (mk-temp-dir dir)
    (let [shallow-dir (mk-tmpfs dir shallow-fs1)]
      (testing "Simple, shallow (single-level) matching"
	(are [pattern files] (= (map #(.getName %)
				     (glob (str dir-name (first shallow-fs1)
						"/"  pattern)))
				files)
	     "*"            ["cat.jpg" "dog.jpg" "lol.gif"]
	     "*.*"          ["cat.jpg" "dog.jpg" "lol.gif"]
	     ".*"           [".hidden"]
	     "*.jpg"        ["cat.jpg" "dog.jpg"]
	     "*.{jpg,gif}"  ["cat.jpg" "dog.jpg" "lol.gif"]))
      (testing "ignore case"
	(are [pattern files] (= (map #(.getName %)
				     (glob (str dir-name (first shallow-fs1)
						"/"  pattern) :i))
				files)
	     "*.JPG"        ["cat.jpg" "dog.jpg"]
	     "*.{JPG,GIF}"  ["cat.jpg" "dog.jpg" "lol.gif"]))
      (testing "option treat dot file fairly"
	(are [pattern files] (= (map #(.getName %)
				     (glob (str dir-name (first shallow-fs1)
						"/"  pattern) :a))
				files)
	     "*"            [".hidden" "cat.jpg" "dog.jpg" "lol.gif"]))
      (rm-rf shallow-dir :silently))
    (let [deep-dir (mk-tmpfs dir deep-fs1)]
      (testing "Deep (multi-level) matching"
	(are [pattern files] (= (map #(.getName %)
				     (glob (str dir-name (first deep-fs1)
						"/"  pattern)))
				files)
	     "*"         ["usr"]
	     "usr/*"     ["bin" "lib" "sbin" "share"]
	     "usr/*/se*" ["sed" "segedit" "sendmail"]
	     "*/*/a*"    ["awk" "arp"]
	     "**/s*a*"   ["share" "samba" "sendmail"]
	     "**AA"      []
	     "*/*/*/*"   ["man1" "man2" "man3"]))
      (rm-rf deep-dir :silently))
    (let [deep-dir2 (mk-tmpfs dir deep-fs2)]
      (testing "Deep (multi-level) matching2"
	(are [pattern files] (= (map #(.getName %)
				     (glob (str dir-name (first deep-fs2)
						"/"  pattern)))
				files)
	     "**clj"     ["a.clj"]
	     "**/*clj"   ["a.clj" "b.clj" "c.clj"]
	     "l**"       ["lib" "lab" "lob"]))
      (rm-rf deep-dir2 :silently))
    (rm-rf dir :silently)))