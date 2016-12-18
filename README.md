# epicea-optional

Adds optional expressions to Clojure.

Summary of three most important commands.
* Use ```(either expr*)``` to choose the first defined expression among a set of optional expressions.
* Use ```(expect fun? expr)``` to define an optional expression ```expr``` whenever ```(fun? expr)``` is true.
* Use ```(optionally pred expr)``` to define an optional expression ```expr``` whenever ```pred``` is true.

## Usage

Example of BMI calculator:
```clojure
(defn compute-bmi-sub [weight height]
  (/ weight (* height height)))

(defn compute-bmi [person]
  (either (compute-bmi-sub (expect number? (:weight person))
                           (expect number? (:height person)))
          nil))

(compute-bmi {:weight 80 :height 1.73}) 
;; 26.729927495071667
(compute-bmi {:height 1.73})
nil
```

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
