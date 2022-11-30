(ns ivarref.pom-patch
  (:require [babashka.process :refer [$ check]]
            [babashka.process.pprint]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip])
  (:import (java.util.regex Pattern)))

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

(defn commit-count []
  (-> ^{:out :string} ($ git rev-list --count HEAD) check :out str/split-lines first ->number))

(defn git-sha []
  (-> ^{:out :string} ($ git rev-parse --short=7 HEAD) check :out str/split-lines first))

(defn set-patch-version! [{:keys [input-file output-file
                                  patch
                                  version-prefix]
                           :or   {input-file     "pom.xml"
                                  output-file    "pom.xml"
                                  version-prefix "v"}
                           :as   opts}]
  (with-open [input (io/input-stream (io/file input-file))]
    (let [root (zip/xml-zip (xml/parse input))
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
                     :or   {input-file     "pom.xml"
                            output-file    "pom.xml"
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

(defn get-quote-value [line k]
  (cond
    (str/starts-with? line k)
    (let [rst (subs line (count k))]
      (str/join "" (->> (seq rst)
                        (drop-while #(contains? #{\" \space} %))
                        (take-while #(not= \" %)))))
    (= 0 (count line))
    nil

    :else
    (get-quote-value (subs line 1) k)))

(defn replace-quote-value [line k new-val]
  (cond
    (not (str/includes? line k))
    line

    (str/starts-with? line k)
    (let [rst (subs line (count k))
          rest-of-line (str/join "" (->> (seq rst)
                                         (drop-while #(contains? #{\" \space} %))
                                         (drop-while #(not (contains? #{\" \space} %)))
                                         (drop-while #(contains? #{\" \space} %))))]
      (str k
           " \""
           new-val
           "\""
           (when-not (str/starts-with? rest-of-line "}")
             " ")
           rest-of-line))
    (= 0 (count line))
    nil

    :else
    (str (first line) (replace-quote-value (subs line 1) k new-val))))


(comment
  (count "8b242e8"))
(comment
  (replace-quote-value
    "com.github.sikt-no/datomic-testcontainers {:git/tag \"0.1.1\" :git/sha \"8b242e8\"}"
    ":git/tag"
    "janei"))

(defn get-kv [inp what]
  (first (mapcat (fn [lin]
                   (when (str/includes? lin what)
                     [(get-quote-value lin what)]))
                 (str/split-lines inp))))

(defn update-readme! [{:keys [git-cmd input-md out-file dry?]
                       :or   {git-cmd  "git"
                              out-file "README.md"
                              dry?     false}}]
  (println "Updating README.md")
  (let [input-md (or input-md (slurp "README.md"))
        [major minor _patch :as old-tag] (str/split (get-kv input-md ":git/tag")
                                                    (Pattern/compile (Pattern/quote ".")))
        new-tag (str major "." minor "." (commit-count))
        git-sha-str (git-sha)
        update-line (fn [line]
                      (-> line
                          (str/replace "NEXT_TAG" new-tag)
                          (replace-quote-value ":git/tag" new-tag)
                          (replace-quote-value ":git/sha" git-sha-str)))
        org-lines (str/split-lines input-md)
        git-cmd (if dry? "echo" git-cmd)
        lines (->> org-lines
                   (mapv update-line)
                   (vec))]
    (println "Creating new git tag" new-tag "for sha" git-sha-str)
    (-> ^{:out :string} ($ ~git-cmd tag -a -m ~(str "Release " new-tag) ~new-tag) check :out println)
    (-> ^{:out :string} ($ ~git-cmd push origin ~new-tag) check :out println)
    (when (string? out-file)
      (if dry?
        (doseq [lin lines]
          (when-not (contains? (into #{} org-lines) lin)
            (println "Updated line:" lin)))
        (spit out-file (str (str/join "\n" lines) "\n")))
      (-> ^{:out :string} ($ ~git-cmd add ~out-file) check :out println)
      (-> ^{:out :string} ($ ~git-cmd commit -m ~(str "Release " new-tag)) check :out println)
      (-> ^{:out :string} ($ ~git-cmd push) check :out println))
    nil))

(comment
  (update-readme!
    {:dry?     true
     :out-file "README.md"
     :input-md (slurp (str "/home/ire/code/datomic-testcontainers" "/README.md"))}))

(comment
  (set-patch-version! {:output-file :repl
                       :patch       :commit-count+1}))

(comment
  (clojars-repo-only! {:input-file  "pom1.xml"
                       :output-file "pom2.xml"}))
