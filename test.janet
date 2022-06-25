(use ./unify)

(defn adder
  [x y]
  (def plus +)
  (plus x y))

(pp
(unify
  '[?x ?y ?plus ?body ?body]
  '[?x ?y ?+    ?plus (?ret ?x ?y)]
  @{'?+ '(Number Number Number)}))