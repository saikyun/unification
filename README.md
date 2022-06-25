# unification

## trying it

```
git clone https://github.com/saikyun/unification
cd unification
janet examples/example.janet
#=>
defn form:
(defn adder
  [x y]
  (+ x y))
  
type of defn:
((?adder             ?+)
 ((?x ?y => ?ret-?+) (?x ?y => ?ret-?+)))

types:
@{?+ (Number Number => Number)
  ?adder (?x ?y => ?ret-?+)
  ?ret-?+ Number
  ?x Number
  ?y Number}
```

## okay, but what does it mean?

1. It parses the `defn`-form
2. Figures out the types in that form
3. Unifies all types, including the predefined environment `@{'?+ '(Number Number => Number)}`
