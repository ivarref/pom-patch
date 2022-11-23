(ns ivarref.pom-patch
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [babashka.process :refer [$ check]]))

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

(defn file->current-version [input-file]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))]
      (->> root
           (zip/children)
           (remove string?)
           (filter #(= :version (:tag (simplify-node %))))
           (first)
           :content
           (str/join "")))))

(defn match-scm-tag? [loc]
  (let [{:keys [tag]} (simplify-node (zip/node loc))]
    (and (= :tag tag)
         (= :scm (:tag (simplify-node (zip/node (zip/up loc))))))))

(defn update-tag! [version-prefix input-str]
  (with-open [input (io/reader (char-array input-str))]
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
                                          (assoc node :content (list (str version-prefix current-version)))))
                           (xml/indent-str)
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n"))]
      new-content)))

(defn ->number [^String s]
  (Long/valueOf s))

(defn set-patch-version! [{:keys [input-file output-file
                                  patch
                                  version-prefix]
                           :or   {input-file  "pom.xml"
                                  output-file "pom.xml"
                                  version-prefix "v"}
                           :as   opts}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
          commit-count (fn [] (-> ^{:out :string} ($ git rev-list --count HEAD) check :out str/split-lines first ->number))
          patch (cond (= :commit-count+1 patch)
                      (inc (commit-count))
                      (= :commit-count patch)
                      (commit-count)
                      :else
                      patch)
          new-content (->> (tree-editor root match-version? (partial editor (str patch)))
                           (xml/indent-str)
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n")
                           (update-tag! version-prefix))]
      (if (= :repl output-file)
        new-content
        (do
          (shutdown-agents)
          (spit output-file new-content)
          (println (file->current-version output-file)))))))

(defn set-version [v node]
  (assoc node :content (list (str v))))

(defn set-version! [{:keys [input-file output-file version-prefix version]
                     :or   {input-file  "pom.xml"
                            output-file "pom.xml"
                            version-prefix "v"}}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
          new-content (->> (tree-editor root match-version? (partial set-version (str version)))
                           (xml/indent-str)
                           (str/split-lines)
                           (remove (comp empty? str/trim))
                           (str/join "\n")
                           (update-tag! version-prefix))]
      (if (= :repl output-file)
        new-content
        (do
          (shutdown-agents)
          (spit output-file new-content)
          (println (file->current-version output-file)))))))

(comment
  (set-version! {:output-file :repl :version "DEV"}))

(defn get-version [{:keys [input-file]
                    :or   {input-file "pom.xml"}
                    :as   opts}]
  (println (file->current-version input-file)))

(comment
  (set-patch-version! {:output-file :repl
                       :patch       :commit-count+1}))

(comment
  (clojars-repo-only! {:input-file  "pom1.xml"
                       :output-file "pom2.xml"}))
