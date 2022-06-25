# can only run this code from freja

(use freja/flow)

(defn render
  [_]
  (draw-rectangle 30 30 50 100 :blue))

(start-game render)

(import ../unify/unify :as u)

(setdyn :pretty-format "%.40M")


(def start-env @{'?start-game '((Dictionary => Void) => Void)})

(def env (u/code->bindings '(defn render
                              [_]
                              (draw-rectangle 30 30 50 100 :blue))

                           start-env))

(print "first success!")

(u/code->bindings '(start-game render) env)
