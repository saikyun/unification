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

(defn parse-call
  [[f & args]]
  (cond
    (= f 'defn)
    (parse-defn [f ;args])

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

(varfn type-of-expr
  [expr]
  (def {:type type} expr)

  (cond
    (= :defn type)
    (type-of-defn expr)

    (= :call type)
    (type-of-call expr)

    (errorf "can't figure out type of expr: %P" expr)))