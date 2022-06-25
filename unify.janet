(defn variable?
  [s]
  (and (symbol? s) (= (chr "?") (first s))))

(varfn unify [& args])

(defn occurs-check?
  [variable s bindings]
  (cond
    (= variable s) true

    (and (variable? s)
         (bindings s))
    (occurs-check? variable (bindings s) bindings)

    (and (indexed? s) (not (empty? s)))
    (or (occurs-check? variable (first s) bindings)
        (occurs-check? variable (drop 1 s) bindings))

    :else false))

(defn unify-variable
  [variable s bindings]
  (cond (bindings variable)
    (unify (bindings variable) s bindings)

    (and (variable? s) (bindings s))
    (unify variable (bindings s) bindings)

    (occurs-check? variable s bindings) (error (string/format "fail: %p occurs in %p" variable s))

    :else
    (put bindings variable s)))

(varfn unify
  [expr1 expr2 &opt bindings]
  (default bindings @{})
  (cond
    (= expr1 expr2)
    bindings

    (variable? expr1)
    (unify-variable expr1 expr2 bindings)

    (variable? expr2)
    (unify-variable expr2 expr1 bindings)

    (and (indexed? expr1)
         (indexed? expr2))
    (if (and (empty? expr1) (empty? expr2))
      bindings
      (->> bindings
           (unify (first expr1)
                  (first expr2))
           (unify (drop 1 expr1) (drop 1 expr2))))

    :error (error (string/format "fail: %p can't be unified with %p" expr1 expr2))
    #
))

(assert (deep= @{'?a 20} (unify '?a 20)))
(assert (deep= @{'?a 20} (unify 20 '?a)))
(assert (deep= (unify ['?a 10] [20 '?b]) (unify ['?a 10] [20 '?b])))
(assert (deep= @{'?x '?y '?a '?y} (unify '(?x ?y ?a) '(?y ?x ?x))))
(assert (deep= @{'?x '?a '?y (quote (+ 5 ?x))}
               (unify ['?x '?y '?a] ['?a '(+ 5 ?x) '?x])))


(assert (deep= @{'?a 10 '?b 15} (unify ['?b ['?b + '?a]] [15 ['?b + 10]])))


# (pp (unify ['?a '?b] [20 '(10 '?b)]))
# (unify ['?a '?a] [10 20])
# ^ both of these are correctly throwing errors
