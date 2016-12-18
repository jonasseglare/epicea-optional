# epicea-optional

Adds optional expressions to Clojure.

Summary of three most important commands.
* Use ```(either expr*)``` to choose the first defined expression among a set of optional expressions.
* Use ```(expect fun? expr)``` to define an optional expression ```expr``` whenever ```(fun? expr)``` is true.
* Use ```(optionally pred expr)``` to define an optional expression ```expr``` whenever ```pred``` is true.

*See the unittests* for a rich set of examples of how this library can be used.

## Usage

Example of BMI calculator:
```clojure
(defn compute-bmi-sub [weight height]
  (/ weight (* height height)))

(defn compute-bmi [person]
  (either (compute-bmi-sub (expect number? (:weight person))
                           (expect number? (:height person)))
          nil))

(compute-bmi {:weight 74 :height 1.73}) 
;; 24.725182932941294
(compute-bmi {:height 1.73})
nil
```

## Discussion

Clojure functions frequently return nil as a result to signal the absence of a value. Checking the return value of every computation is a lot of work. This library aims at taking away some of that pain by providing control flow utilites and the concept of *optional* expressions that are only true if a condition holds. And consequently, any computation that depends on an optional expression becomes itself an optional expression. The ```either``` macro can then be used to choose the first defined value from a set of optional expressions.

The concept of optionals is also present in other languages, such as Swift, which also provides some control flow constructs to make programming with optionals easier, such as optional chaining. With this libary, optionals can appear anywhere in an expression and is not limited in the same way as it is in Swift.

Another point is that it may facilitate to use only return values to communicate the result of a computation, where exceptions otherwise would be used. Because the exceptional flow resulting from exceptions can be very hard to follow, I try to avoid them and only use return values. Exceptions can still be useful though for indicating bugs, e.g. through assertions.

## License

Copyright © 2016 Jonas Östlund

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
