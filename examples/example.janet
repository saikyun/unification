(use ../unify/unify)
(use ../unify/parse)

(let [env @{'?+ '(Number Number => Number)}
      adder-form '(defn adder
                    [x y]
                    (+ x y))
      parsed-adder-form (parse-defn adder-form)
      types-of-adder (type-of-defn parsed-adder-form)]

  (printf "defn form: %P" adder-form)

  (printf "type of defn: %P" types-of-adder)

  (comment
    # this is what we want to get
    (unify ['?+ '?adder]
           ['(?x ?y => ?body) '(?x ?y => ?body)]
           env))

  (printf "types: %P" (unify-kvs
                        (first types-of-adder) env))
  #
)
