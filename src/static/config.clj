(ns static.config
  (:use [clojure.tools logging])
  (:import (java.io File)))

(let [defaults {:site-title "A Static Blog"
                :site-description "Default blog description"
                :site-url "https://github.com/nakkaya/static"
                :in-dir "resources/"
                :out-dir "html/"
                :post-out-subdir ""
                :default-template "default.clj"
                :snippet-template "snippet.clj"
                :default-extension "html"
                :encoding "UTF-8"
                :posts-per-page 2
                :blog-as-index true
                :create-archives true
                :org-export-command '(progn 
                                      (org-html-export-as-html nil nil nil t nil)
                                      (with-current-buffer "*Org HTML Export*"
                                        (princ (org-no-properties (buffer-string)))))}]
  (def config
    (memoize
     (fn [& [config-file]]
       (try
         (let [config (apply hash-map
                             (read-string
                              (slurp (File. (or config-file "config.clj")))))]
           ;;if emacs key is set make sure executable exists.
           (when (and (:emacs config) (not (.exists (File. (:emacs config)))))
             (error "Path to Emacs not valid.")
             (System/exit 0))
           (merge defaults config))
         (catch Exception e
           (info "Configuration not found using defaults.")
           defaults))))))

(defn set!-config [k v]
  (alter-var-root (find-var 'static.config/config) (fn [c] #(identity (assoc (c) k v)))))
