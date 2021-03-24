(ns ivarref.pom-patch
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]))

; adapted from https://ravi.pckl.me/short/functional-xml-editing-using-zippers-in-clojure/

(defn tree-edit
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   apply the editor."
  [zipper matcher]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher loc)]
        (recur (zip/remove loc))
        (recur (zip/next loc))))))

(defn simplify-node [n]
  (walk/prewalk
    (fn [x]
      (if (keyword? x)
        (keyword (name x))
        x))
    n))

(defn match-repository? [loc]
  (let [{:keys [tag content]} (simplify-node (zip/node loc))]
    (when (= :repository tag)
      (->> content
           (filter map?)
           (filter #(= :url (:tag %)))
           (first)
           :content
           (str/join "")
           (str/trim)
           (not= "https://repo.clojars.org/")))))

(defn clojars-repo-only! [{:keys [input-file output-file]
                           :or   {input-file  "pom.xml"
                                  output-file "pom.xml"}}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
          new-content (->> (xml/indent-str (tree-edit root match-repository?))
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n"))]
      (spit output-file new-content))))


(defn tree-editor
  "Take a zipper, a function that matches a pattern in the tree,
   and a function that edits the current location in the tree.  Examine the tree
   nodes in depth-first order, determine whether the matcher matches, and if so
   apply the editor."
  [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher loc)]
        (recur (zip/next (zip/edit loc editor)))
        (recur (zip/next loc))))))


(defn match-version? [loc]
  (let [{:keys [tag]} (simplify-node (zip/node loc))]
    (and (= :version tag)
         (= :project (:tag (simplify-node (zip/node (zip/up loc))))))))

(defn editor [new-patch node]
  (let [curr-content (str/join "" (:content node))
        [major minor _old-patch] (str/split curr-content #"\.")]
    (assoc node :content (list (str major
                                    "."
                                    minor
                                    "."
                                    new-patch)))))

(defn set-patch-version! [{:keys [input-file output-file
                                  patch]
                           :or   {input-file  "pom.xml"
                                  output-file "pom.xml"}}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
          new-content (->> (tree-editor root match-version? (partial editor (str patch)))
                           (xml/indent-str)
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n"))]
      (if (= :repl output-file)
          new-content
          (spit output-file new-content)))))

(defn match-scm-tag? [loc]
  (let [{:keys [tag]} (simplify-node (zip/node loc))]
    (and (= :tag tag)
         (= :scm (:tag (simplify-node (zip/node (zip/up loc))))))))

(defn update-tag! [{:keys [input-file output-file]
                    :or   {input-file  "pom.xml"
                           output-file "pom.xml"}}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
          current-version (->> root
                               (zip/children)
                               (remove string?)
                               (filter #(= :version (:tag (simplify-node %))))
                               (first)
                               :content
                               (str/join ""))
          new-content (->> (tree-editor root match-scm-tag?
                                        (fn [node]
                                          (assoc node :content (list (str "v" current-version)))))
                           (xml/indent-str)
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n"))]
      (if (= :repl output-file)
        new-content
        (spit output-file new-content)))))

(comment
  (update-tag! {:input-file  "pom.xml"
                :output-file :repl}))

(comment
  (clojars-repo-only! {:input-file  "pom1.xml"
                       :output-file "pom2.xml"}))