(use ./unify)

## example function

(defn adder
  [x y]
  (+ x y))


## parsing

(defn ->var
  [s]
  (symbol (string "?" s)))

(defn parse-call
  [[f & args]]
  {:type :call
   :f (->var f)
   :args (map ->var args)})

(defn parse-body
  [body]
  (if (tuple? body)
    (parse-call body)
    (errorf "can't parse body: %P" body)))

(defn parse-defn
  [[_ name args body]]
  {:type :defn
   :name (->var name)
   :args (map ->var args)
   :body (parse-body body)})


## figuring out types of symbols

# defn's type is '(arg1 arg2 ... argn => body)
# call's type is (last (typeof :f))
# call's arguments types are (take n (typeof :f))
#  where n is nof args

(defn type-of-call
  [{:f f :args args}]
  (let [ret (symbol (string "?ret-" (string f)))]
    [[f]
     [[;args '=> ret]]
     ret]))

(defn type-of-defn
  [{:name name :args args :body body}]
  (let [[body-fst body-snd ret] (type-of-call body)]
    [[name ;body-fst]
     [[;args '=> ret] ;body-snd]]))


(let [env @{'?+ '(Number Number => Number)}
      adder-form (parse-defn '(defn adder
                                [x y]
                                (+ x y)))
      types-of-adder (type-of-defn adder-form)]

  (printf "type of defn: %P" types-of-adder)

  (comment
    # this is what we want to get
    (unify ['?+ '?adder]
           ['(?x ?y => ?body) '(?x ?y => ?body)]
           env))

  (printf "types: %P" (unify ;types-of-adder env))
  #
)
