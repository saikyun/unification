# can only run this code from freja

(use freja/flow)

(defn render
  [_]
  (draw-rectangle 30 30 50 100 :blue))

# (start-game render)

(import ../unify/unify :as u :fresh true)

(setdyn :pretty-format "%.40M")


(def start-env @{'?start-game-args '{?:width Number}
                 '?start-game '((?start-game-args => Void) => Void)})

(def env (u/code->bindings '(start-game render) start-env))

(def env (u/code->bindings
            '(defn render
               [data]
               (let [w (get data :width)]
                 (draw-rectangle 30 30 50 100 :blue)))

            env))

(print "first success!")

# (u/code->bindings '(start-game render) env)

