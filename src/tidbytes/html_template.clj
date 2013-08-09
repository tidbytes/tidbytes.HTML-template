(ns tidbytes.html-template)


(defmacro to-html 
  "Docstring coming soon!"
  [& args]
  (defn handle-attr
    "Takes a map of html attributes/value and returns them as a string"
    [attrmap]
    (let [attributes (keys attrmap)]
      (apply str (for [attr (vec attributes)]
                   (format " %s=\"%s\""
                           (apply str (rest (str attr)))
                           (attrmap attr))))))
  (defn denest-html
    "Takes a vector consisting of blocks of code and passes them to format-html in the appropriate order."
    [&[args]]
    (defn format-html
      "Takes a clojure list as argument, 'translates' it into html and returns the resulting string."
      [expr]
      (cond
        (seq? expr)
          (let [tag (identity (first expr))
                content (vec (rest expr))]
            (cond
              (= (symbol "clojure.core/unquote") tag)
                (str (eval (first content)))
              (and (= (count content) 1)(map? (first content)))
                (format "<%s%s />"
                        tag (handle-attr (first content)))
              (map? (first content))
                (format "<%s%s>%s</%s>"
                        tag (handle-attr (first content))
                        (denest-html content)
                        tag)
              (= (count content) 0)
                (format "<%s />" tag)
              (string? (first content))
                (format "<%s>%s</%s>"
                        tag (denest-html content) tag)
              :else
                (format "<%s>%s</%s>"
                        tag (denest-html content) tag)))
        (symbol? expr)
          (format "<%s />"
                  expr)
        (string? expr) expr))
    (apply str (for [arg args]
                 (format-html arg))))
  `(let [~'input (vec '~args)]
     (denest-html ~'input)))