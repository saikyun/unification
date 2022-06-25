## parsing

(defn hole?
  [x]
  (and (symbol? x)
       (= (chr "?") (first (string x)))))

(defn ->hole
  [s]
  (symbol (string "?" s)))

(varfn parse-expr [expr])

(defn parse-defn
  [[_ name args body]]
  {:type :defn
   :name (->hole name)
   :args (map ->hole args)
   :body (parse-expr body)})

(defn parse-kv
  [k v]
  [(->hole k) (parse-expr v)])

(defn parse-bindings
  [bindings]
  (let [[k v] (take 2 bindings)
        rest (drop 2 bindings)]
    ;[(parse-kv k v)
      ;(if (empty? rest)
         []
         (parse-bindings rest))]))

(defn parse-let
  [[_ bindings body]]
  {:type :let
   :name (->hole 'let)
   :bindings (parse-bindings bindings)
   :body (parse-expr body)})

(defn parse-get
  [[_ container kw]]
  {:type :get
   :dict (parse-expr container)
   :key (->hole (string ":" kw))})

(defn parse-call
  [[f & args]]
  (cond
    (= f 'defn)
    (parse-defn [f ;args])

    (= f 'let)
    (parse-let [f ;args])

    (= f 'get)
    (parse-get [f ;args])

    :else
    {:type :call
     :f (->hole f)
     :args (map parse-expr args)}))

(defn parse-number
  [n]
  {:type 'Number
   :value n})

(defn parse-keyword
  [k]
  {:type 'Keyword
   :value k})

(varfn parse-expr
  [expr]
  (cond (tuple? expr)
    (parse-call expr)

    (number? expr)
    (parse-number expr)

    (keyword? expr)
    (parse-keyword expr)

    (symbol? expr)
    (->hole expr)

    (errorf "can't parse expr: %p" expr)))

## figuring out types of symbols

# defn's type is '(arg1 arg2 ... argn => body)
# call's type is (last (typeof :f))
# call's arguments types are (take n (typeof :f))
#  where n is nof args

(defn type-or-hole
  [x]
  (cond (and (dictionary? x) (x :type))
    (x :type)

    (hole? x)
    x

    (errorf "is not dict with type nor hole: %P" x)))

(varfn type-of-expr [expr])

(defn type-of-call
  [{:f f :args args}]
  (let [ret (symbol (string "?ret-" (string f)))]
    [[[f [;(map type-or-hole args) '=> ret]]]
     ret]))

(defn type-of-defn
  [{:name name :args args :body body}]
  (let [[types ret] (type-of-expr body)]
    [[[name [;args '=> ret]]
      ;types]
     ret]))

(defn type-of-let
  [{:name name :bindings bindings :body body}]
  (let [[types ret] (type-of-expr body)]
    [[;(tracev (mapcat
                 (fn [[k v]]

                   (let [[types ret] (type-of-expr v)]
                     [[k ret]
                      ;types])) bindings))
      ;types]
     ret]))

(defn type-of-get
  [{:dict dict :key kw}]
  [[[dict {kw '?get-type}]]
   '?get-type])

(varfn type-of-expr
  [expr]
  (def {:type type} expr)

  (cond
    (= :let type)
    (type-of-let expr)

    (= :defn type)
    (type-of-defn expr)

    (= :get type)
    (type-of-get expr)

    (= :call type)
    (type-of-call expr)

    (errorf "can't figure out type of expr: %P" expr)))
